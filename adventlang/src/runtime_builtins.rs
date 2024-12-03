use crate::runtime::FnSig;

pub trait RuntimeLike {
    fn builtin(&mut self, name: &str, signatures: impl IntoIterator<Item = FnSig>);
}

pub struct RuntimeBuiltinDefs {
    pub builtins: Vec<(String, Vec<FnSig>)>,
}

impl RuntimeBuiltinDefs {
    pub fn new() -> Self {
        Self { builtins: vec![] }
    }
}

impl RuntimeLike for RuntimeBuiltinDefs {
    fn builtin(&mut self, name: &str, signatures: impl IntoIterator<Item = FnSig>) {
        self.builtins
            .push((name.into(), signatures.into_iter().collect()));
    }
}
