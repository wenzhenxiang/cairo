// TODO(yg): move to graph algos?

use std::collections::HashMap;

use cairo_lang_semantic::corelib::{core_submodule, get_core_function_id, get_function_id};
use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;

use crate::blocks::{FlatBlocks, FlatBlocksBuilder};
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchInfo, Statement, VarRemapping, VariableId,
};

pub enum WithdrawGasStatus {
    WithdrawGasAll,
    WithdrawGas,
    None,
}

pub fn find_withdraw_status(
    db: &dyn LoweringGroup,
    lowered: &mut FlatLowered,
) -> WithdrawGasStatus {
    let semantic_db = db.upcast();
    let mandatory_blocks = find_mandatory_blocks(lowered);
    let gas_module = core_submodule(semantic_db, "gas");
    let withdraw_gas_all_id =
        get_function_id(semantic_db, gas_module, "withdraw_gas_all".into(), vec![]);
    let withdraw_gas_id = get_function_id(semantic_db, gas_module, "withdraw_gas".into(), vec![]);

    let mut has_withdraw_gas = false;
    for block_id in mandatory_blocks {
        let block = &lowered.blocks[block_id];
        for stmnt in &block.statements {
            if let Statement::Call(call) = stmnt {
                if call.function.is_semantic_and_eq(db, withdraw_gas_all_id) {
                    return WithdrawGasStatus::WithdrawGasAll;
                } else if call.function.is_semantic_and_eq(db, withdraw_gas_id) {
                    has_withdraw_gas = true;
                }
            }
        }
    }

    if has_withdraw_gas { WithdrawGasStatus::WithdrawGas } else { WithdrawGasStatus::None }
}

// TODO(yg): make this a query?
pub fn find_mandatory_blocks(lowered: &mut FlatLowered) -> Vec<BlockId> {
    if lowered.blocks.is_empty() {
        return Vec::new();
    }

    let ctx = MandatoryCallsContext { incoming_gotos: vec![0; lowered.blocks.len()] };
    let mut analysis =
        BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: ctx };
    analysis.get_root_info();
    let ctx = analysis.analyzer;

    let my_blocks = ctx
        .incoming_gotos
        .iter()
        .map(|num_parents| MyBlock { current_weight: 0.0, parents_left: *num_parents })
        .collect_vec();
    let mut my_ctx = MyContext { my_blocks, lowered_blocks: &lowered.blocks };
    update_block_weight(&mut my_ctx, BlockId::root(), 1.0);

    let mandatory_blocks = my_ctx
        .my_blocks
        .into_iter()
        .enumerate()
        .filter_map(
            |(idx, my_block)| {
                if my_block.current_weight == 1.0 { Some(BlockId(idx)) } else { None }
            },
        )
        .collect_vec();
    mandatory_blocks
}

// TODO(yg): rename.
#[derive(Clone)]
struct MyBlock {
    current_weight: f32,
    parents_left: usize,
}

// TODO(yg): rename.
struct MyContext<'a> {
    my_blocks: Vec<MyBlock>,
    lowered_blocks: &'a FlatBlocks,
}

// TODO(yg): context with lowered and my_blocks.
// TODO(yg): f## is not accurate enough to get exactly to 1... e.g. 1/3+1/3+1/3 != 1. collect parts.
// let mul = parts.mul() and check that parts.map(|part| mul/part).sum() == mul.
fn update_block_weight(ctx: &mut MyContext, block_id: BlockId, weight_from_parent: f32) {
    let my_block = &mut ctx.my_blocks[block_id.0];
    my_block.current_weight += weight_from_parent;
    my_block.parents_left -= 1;
    if my_block.parents_left > 0 || my_block.current_weight < 1.0 {
        return;
    }

    let block = ctx.lowered_blocks[block_id].clone();
    // Recursive call for all children.
    match block.end {
        FlatBlockEnd::NotSet => unreachable!(),
        FlatBlockEnd::Return(_) | FlatBlockEnd::Panic(_) => {}
        FlatBlockEnd::Goto(child_id, _) => update_block_weight(ctx, child_id, 1.0),
        FlatBlockEnd::Match { info } => {
            let arms = info.arms();
            let child_weight = 1.0 / arms.len() as f32;
            for arm in arms {
                update_block_weight(ctx, arm.block_id, child_weight);
            }
        }
    }
}

struct MandatoryCallsContext {
    /// The number of incoming gotos, indexed by block_id.
    incoming_gotos: Vec<usize>,
}

impl Analyzer<'_> for MandatoryCallsContext {
    type Info = ();

    fn visit_block_start(&mut self, _info: &mut Self::Info, block_id: BlockId, _block: &FlatBlock) {
    }

    fn visit_remapping(
        &mut self,
        _info: &mut Self::Info,
        _statement_location: StatementLocation,
        target_block_id: BlockId,
        _remapping: &VarRemapping,
    ) {
        self.incoming_gotos[target_block_id.0] += 1;
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        _infos: &[Self::Info],
    ) -> Self::Info {
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        _vars: &[VariableId],
    ) -> Self::Info {
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VariableId,
    ) -> Self::Info {
    }
}
