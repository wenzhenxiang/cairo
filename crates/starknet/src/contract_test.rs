use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::{setup_test_crate, SemanticDatabaseForTesting};

use crate::contract::find_contract_structs;

#[test]
fn test_no_contract() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let _crate_id = setup_test_crate(
        db,
        indoc! {" struct NotAContract {}
        }


        #[contract(ERC20Impl)]
        struct ERC20 {}


        #[contract(ERC20Impl)]
        struct YetAnotherContract {}
        
        
        
        "},
    );

    assert_eq!(format!("{:?}", find_contract_structs(db)), "[StructId(1), StructId(2)]");
}
