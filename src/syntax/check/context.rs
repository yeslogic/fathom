//! The type checking context and binders

use name::Named;
use syntax::ast::{binary, host};
use var::{BoundVar, ScopeIndex};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope<N> {
    ExprAbs(Vec<Named<N, host::RcType<N>>>),
    TypeAbs(Vec<Named<N, binary::Kind>>),
    TypeDef(Vec<Named<N, (binary::RcType<N>, binary::Kind)>>),
}

#[derive(Debug, Clone)]
pub struct Context<N> {
    scopes: Vec<Scope<N>>,
}

impl<N> Context<N> {
    pub fn new() -> Context<N> {
        Context { scopes: Vec::new() }
    }

    pub fn extend(&mut self, scope: Scope<N>) {
        self.scopes.push(scope);
    }

    pub fn lookup(&self, scope: ScopeIndex) -> &Scope<N> {
        assert!(
            self.scopes.len() > scope.0 as usize,
            "ICE: Scope out of range"
        );

        &self.scopes[(self.scopes.len() - scope.0 as usize) - 1]
    }

    pub fn lookup_ty(&self, var: BoundVar) -> Result<(&N, &host::RcType<N>), &Scope<N>> {
        match *self.lookup(var.scope) {
            Scope::ExprAbs(ref tys) => Ok(
                tys.get(var.binding.0 as usize)
                    .map(|named| (&named.0, &named.1))
                    .expect("ICE: Binder out of range"),
            ),
            ref scope => Err(scope),
        }
    }

    pub fn lookup_ty_def(&self, var: BoundVar) -> Result<(&N, &binary::RcType<N>), &Scope<N>> {
        match *self.lookup(var.scope) {
            Scope::TypeDef(ref defs) => Ok(
                defs.get(var.binding.0 as usize)
                    .map(|named| (&named.0, &(named.1).0))
                    .expect("ICE: Binder out of range"),
            ),
            ref scope => Err(scope),
        }
    }

    pub fn lookup_kind(&self, var: BoundVar) -> Result<(&N, &binary::Kind), &Scope<N>> {
        match *self.lookup(var.scope) {
            Scope::TypeAbs(ref kinds) => Ok(
                kinds
                    .get(var.binding.0 as usize)
                    .map(|named| (&named.0, &named.1))
                    .expect("ICE: Binder out of range"),
            ),
            Scope::TypeDef(ref defs) => Ok(
                defs.get(var.binding.0 as usize)
                    .map(|named| (&named.0, &(named.1).1))
                    .expect("ICE: Binder out of range"),
            ),
            ref scope => Err(scope),
        }
    }
}
