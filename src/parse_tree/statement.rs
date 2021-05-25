use crate::interner::IValue;
use crate::parse_tree::*;

pub enum Statement {
    Labeled(IValue<String>, Box<Statement>),
    Case(TaggedExpr, Box<Statement>),
    Default,
    Compound(Box<[DeclOrStmnt]>),
    Expr(TaggedExpr),
    If(TaggedExpr, Box<Statement>, Option<Box<Statement>>),
    Switch(TaggedExpr, Box<Statement>),
    DoWhile(Box<Statement>, TaggedExpr),
    While(TaggedExpr, Box<Statement>),
    For { 
        init: TaggedExpr,
        cond: TaggedExpr,
        inc: Option<TaggedExpr>,
        body: Box<Statement>
    },
    Goto(IValue<String>),
    Continue,
    Break,
    Return(Option<TaggedExpr>),
    Nop,
}

impl Statement {
    pub fn pretty_print(&self, buf: &mut String, si: &Interner<String>) {
        use Statement::*;
        match self {
            Labeled(l, s) => {
                buf.push_str(si.get(*l));
                buf.push_str(": ");
                s.pretty_print(buf, si);
            },
            Case(expr, s) => {
                buf.push_str("case ");
                expr.pretty_print(buf, si);
                buf.push_str(": ");
                s.pretty_print(buf, si);
            },
            Default => {
                buf.push_str("default;");
            },
            Compound(body) => {
                buf.push_str("{\n");
                for element in body.iter() {
                    buf.push_str("    ");
                    element.pretty_print(buf, si);
                }
                buf.push_str("\n    }")
            },
            Expr(e) => {
                e.pretty_print(buf, si);
                buf.push(';');
            },
            If(e, true_branch, false_branch) => {
                buf.push_str("if (");
                e.pretty_print(buf, si);
                buf.push_str(")\n    ");
                true_branch.pretty_print(buf, si);
                buf.push_str(";\n");
                if let Some(f) = false_branch {
                    buf.push_str("    else\n    ");
                    f.pretty_print(buf, si);
                    buf.push(';');
                }
            },
            Switch(exp, s) => {
                buf.push_str("switch (");
                exp.pretty_print(buf, si);
                buf.push(')');
                s.pretty_print(buf, si);
            },
            DoWhile(s, exp) => {
                buf.push_str("do ");
                s.pretty_print(buf, si);
                buf.push_str(" while (");
                exp.pretty_print(buf, si);
                buf.push_str(");");
            },
            While(exp, s) => {
                buf.push_str("while (");
                exp.pretty_print(buf, si);
                buf.push_str(") ");
                s.pretty_print(buf, si);
            },
            For { init, cond, inc, body } => {
                buf.push_str("for (");
                init.pretty_print(buf, si);
                buf.push_str("; ");
                cond.pretty_print(buf, si);
                buf.push_str("; ");
                if let Some(i) = inc {
                    i.pretty_print(buf, si);
                }
                buf.push_str(") ");
                body.pretty_print(buf, si);
            },
            Goto(lbl) => {
                buf.push_str("goto ");
                buf.push_str(si.get(*lbl));
                buf.push(';');
            },
            Continue => buf.push_str("continue;"),
            Break => buf.push_str("break;"),
            Return(Some(exp)) => {
                buf.push_str("return ");
                exp.pretty_print(buf, si);
                buf.push(';');
            },
            Return(None) => buf.push_str("return;"),
            Nop => buf.push_str(";"),
        };
        buf.push('\n');
    }
}