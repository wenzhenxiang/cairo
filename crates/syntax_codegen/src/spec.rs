// Representation of the AST specifications.

pub struct Node {
    pub name: String,
    pub kind: NodeKind,
}
pub enum NodeKind {
    Enum { variants: Vec<String>, missing_variant: String },
    Struct { members: Vec<Member> },
    List { element_type: String },
    SeparatedList { element_type: String },
}
pub struct Member {
    pub name: String,
    pub kind: MemberKind,
}
pub enum MemberKind {
    Token,
    Node(String),
}
struct StructBuilder {
    name: String,
    members: Vec<Member>,
}
impl StructBuilder {
    fn new(name: &'static str) -> Self {
        Self { name: name.into(), members: Vec::new() }
    }
    fn node(mut self, name: &'static str, node: &'static str) -> StructBuilder {
        self.members.push(Member { name: name.into(), kind: MemberKind::Node(node.into()) });
        self
    }
    fn token(mut self, name: &'static str) -> StructBuilder {
        self.members.push(Member { name: name.into(), kind: MemberKind::Token });
        self
    }
    fn build(self) -> Node {
        Node { name: self.name, kind: NodeKind::Struct { members: self.members } }
    }
}

fn enum_node(
    name: &'static str,
    missing_variant: &'static str,
    mut variants: Vec<&'static str>,
) -> Node {
    variants.push(missing_variant);
    Node {
        name: name.into(),
        kind: NodeKind::Enum {
            missing_variant: missing_variant.into(),
            variants: variants.into_iter().map(String::from).collect(),
        },
    }
}
fn list_node(name: &'static str, element_type: &'static str) -> Node {
    Node { name: name.into(), kind: NodeKind::List { element_type: element_type.into() } }
}
fn separated_list_node(name: &'static str, element_type: &'static str) -> Node {
    Node { name: name.into(), kind: NodeKind::SeparatedList { element_type: element_type.into() } }
}

#[allow(dead_code)]
pub fn get_spec() -> Vec<Node> {
    let nodes = vec![
        // Empty.
        StructBuilder::new("Empty").build(),
        // Terminal.
        StructBuilder::new("Terminal")
            .node("leading_trivia", "Trivia")
            .token("token")
            .node("trailing_trivia", "Trivia")
            .build(),
        list_node("Trivia", "Trivium"),
        enum_node(
            "Trivium",
            "Empty",
            vec!["TriviumSingleLineComment", "TriviumWhitespace", "TriviumNewline"],
        ),
        StructBuilder::new("TriviumSingleLineComment").token("token").build(),
        StructBuilder::new("TriviumWhitespace").token("token").build(),
        StructBuilder::new("TriviumNewline").token("token").build(),
        // Expressions.
        StructBuilder::new("Identifier").node("terminal", "Terminal").build(),
        enum_node(
            "Expr",
            "ExprMissing",
            vec!["ExprPath", "ExprLiteral", "ExprParenthesized", "ExprUnary", "ExprBinary"],
        ),
        StructBuilder::new("ExprMissing").build(),
        separated_list_node("ExprPath", "PathSegment"),
        StructBuilder::new("PathSegment")
            .node("ident", "Identifier")
            .node("args", "OptionGenericArgs")
            .build(),
        enum_node("OptionGenericArgs", "Empty", vec!["GenericArgs"]),
        separated_list_node("GenericArgs", "Expr"),
        StructBuilder::new("ExprLiteral").node("terminal", "Terminal").build(),
        StructBuilder::new("ExprParenthesized")
            .node("lparen", "Terminal")
            .node("expr", "Expr")
            .node("rparen", "Terminal")
            .build(),
        StructBuilder::new("ExprUnary").node("op", "Terminal").node("expr", "Expr").build(),
        StructBuilder::new("ExprBinary")
            .node("left", "Expr")
            .node("op", "Terminal")
            .node("right", "Expr")
            .build(),
        StructBuilder::new("ExprBlock")
            .node("left", "Terminal")
            .node("statements", "StatementList")
            .node("right", "Terminal")
            .build(),
        // Statements.
        separated_list_node("StatementList", "Statement"),
        enum_node(
            "Statement",
            "StatementMissing",
            vec!["StatementLet", "StatementExpr", "StatementReturn"],
        ),
        StructBuilder::new("StatementMissing").build(),
        StructBuilder::new("StatementLet")
            .node("letkw", "Terminal")
            .node("lhs", "Identifier")
            .node("eq", "Terminal")
            .node("rhs", "Expr")
            .build(),
        StructBuilder::new("StatementExpr").node("expr", "Expr").build(),
        StructBuilder::new("StatementReturn")
            .node("returnkw", "Terminal")
            .node("expr", "Expr")
            .build(),
        // Items.
        separated_list_node("ItemList", "Item"),
        enum_node(
            "Item",
            "Empty",
            vec!["ItemModule", "ItemFunction", "ItemTrait", "ItemImpl", "ItemStruct", "ItemEnum"],
        ),
        StructBuilder::new("ItemModule")
            .node("modkw", "Terminal")
            .node("name", "Identifier")
            .node("semi", "Terminal")
            .build(),
        StructBuilder::new("Semi").node("token", "Terminal").build(),
        enum_node("FuncBody", "Semi", vec!["ExprBlock"]),
        StructBuilder::new("ItemFunction")
            .node("funckw", "Terminal")
            .node("name", "Identifier")
            .node("lparen", "Terminal")
            .node("arguments", "ArgumentList")
            .node("rparen", "Terminal")
            .node("arrow", "Terminal")
            .node("ret_ty", "Expr")
            .node("body", "FuncBody")
            .build(),
        separated_list_node("ArgumentList", "Argument"),
        StructBuilder::new("Argument")
            .node("name", "Identifier")
            .node("colon", "Terminal")
            .node("ty", "Expr")
            .build(),
        StructBuilder::new("ItemTrait")
            .node("kwtrait", "Terminal")
            .node("name", "Identifier")
            .node("left", "Terminal")
            .node("items", "ItemList")
            .node("right", "Terminal")
            .build(),
        StructBuilder::new("ItemImpl")
            .node("kwimpl", "Terminal")
            .node("name", "Identifier")
            .node("of", "Terminal")
            .node("kwtrait", "Identifier")
            .node("kwfor", "Terminal")
            .node("ty", "Identifier")
            .node("left", "Terminal")
            .node("items", "ItemList")
            .node("right", "Terminal")
            .build(),
        StructBuilder::new("ItemStruct")
            .node("kwstruct", "Terminal")
            .node("name", "Identifier")
            .node("left", "Terminal")
            .node("items", "MemberList")
            .node("right", "Terminal")
            .build(),
        separated_list_node("MemberList", "Member"),
        StructBuilder::new("Member")
            .node("name", "Identifier")
            .node("colon", "Terminal")
            .node("ty", "Expr")
            .build(),
        StructBuilder::new("ItemEnum")
            .node("kwenum", "Terminal")
            .node("name", "Identifier")
            .node("left", "Terminal")
            .node("variants", "VariantList")
            .node("right", "Terminal")
            .build(),
        separated_list_node("VariantList", "Variant"),
        StructBuilder::new("Variant").node("ty", "Expr").build(),
        // Meta.
        StructBuilder::new("CompilationUnit")
            .node("items", "ItemList")
            .node("eof", "Terminal")
            .build(),
    ];

    nodes
}
