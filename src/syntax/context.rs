use syntax::{binary, host};
use syntax::Named;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding<N> {
    Expr(host::Type<N>),
    Type(binary::Kind),
    TypeDef(binary::Type<N>, binary::Kind),
}

#[derive(Debug, Clone)]
pub struct Context<N> {
    bindings: Vec<Named<N, Binding<N>>>,
}

impl<N> Context<N> {
    pub fn new() -> Context<N> {
        Context {
            bindings: Vec::new(),
        }
    }

    pub fn extend(&mut self, name: N, binding: Binding<N>) {
        self.bindings.push(Named(name, binding));
    }

    pub fn lookup(&self, i: u32) -> Named<&N, &Binding<N>> {
        assert!(self.bindings.len() > i as usize, "ICE: Binder out of range");

        let Named(ref name, ref binding) = self.bindings[self.bindings.len() - i as usize - 1];

        Named(name, binding)
    }

    pub fn lookup_ty(&self, i: u32) -> Result<Named<&N, &host::Type<N>>, Named<&N, &Binding<N>>> {
        let Named(name, binding) = self.lookup(i);

        match *binding {
            Binding::Expr(ref ty) => Ok(Named(name, ty)),
            _ => Err(Named(name, binding)),
        }
    }

    pub fn lookup_ty_def(
        &self,
        i: u32,
    ) -> Result<Named<&N, &binary::Type<N>>, Named<&N, &Binding<N>>> {
        let Named(name, binding) = self.lookup(i);

        match *binding {
            Binding::TypeDef(ref ty, _) => Ok(Named(name, ty)),
            _ => Err(Named(name, binding)),
        }
    }

    pub fn lookup_kind(&self, i: u32) -> Result<Named<&N, &binary::Kind>, Named<&N, &Binding<N>>> {
        let Named(name, binding) = self.lookup(i);

        match *binding {
            Binding::Type(ref kind) => Ok(Named(name, kind)),
            Binding::TypeDef(_, ref kind) => Ok(Named(name, kind)),
            _ => Err(Named(name, binding)),
        }
    }
}
