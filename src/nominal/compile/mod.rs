//! Compilation from the structural language to the nominal language

use std::rc::Rc;

use nominal::ast::{Path, Program, Type};
use structural;
use structural::ast::{binary, Field};

#[cfg(test)]
mod tests;

pub fn compile_program(src: &structural::ast::Program<String>) -> Program<String> {
    let mut program = Program::new();

    for def in &src.defs {
        // Begin tracking the path of this definition from the root name of the
        // source definition. This will be appended to in order to provide a
        // fully qualified path through the type definitions, eg:
        // `Foo::field::Entry::Variant2::...`
        let path = Path::new(def.name.clone());

        match *def.ty {
            binary::Type::Struct(_, ref fields) => {
                let fields = compile_fields(&mut program, &path, fields);
                program.define_struct(path, fields);
            }
            binary::Type::Union(_, ref variants) => {
                let variants = compile_variants(&mut program, &path, variants);
                program.define_union(path, variants);
            }
            _ => {
                let ty = compile_ty(&mut program, &path, &def.ty);
                program.define_alias(path, ty);
            }
        }
    }

    program
}

fn compile_fields(
    program: &mut Program<String>,
    path: &Path<String>,
    fields: &[Field<String, binary::RcType<String>>],
) -> Vec<Field<String, Type<String>>> {
    fields
        .iter()
        .map(|field| {
            let field_path = path.append_child(field.name.clone());
            let ty = compile_ty(program, &field_path, &field.value);

            Field::new(field.name.clone(), ty)
        })
        .collect()
}

fn compile_variants(
    program: &mut Program<String>,
    path: &Path<String>,
    variants: &[binary::RcType<String>],
) -> Vec<Type<String>> {
    variants
        .iter()
        .enumerate()
        .map(|(i, variant_ty)| {
            let variant_path = path.append_child(format!("Variant{}", i));
            compile_ty(program, &variant_path, &variant_ty)
        })
        .collect()
}

fn compile_ty(
    program: &mut Program<String>,
    path: &Path<String>,
    ty: &binary::RcType<String>,
) -> Type<String> {
    use name::Named;
    use structural::ast::Var;

    match **ty {
        binary::Type::Var(_, Var::Bound(Named(ref name, _))) => Type::path(name.as_str()),
        binary::Type::Var(_, Var::Free(_)) => unimplemented!(),
        binary::Type::Const(_) => unimplemented!(),
        binary::Type::Array(_, ref elem_ty, ref size_expr) => {
            let elem_path = path.append_child("Elem");
            let elem_ty = compile_ty(program, &elem_path, elem_ty);

            Type::array(elem_ty, size_expr.clone())
        }
        binary::Type::Union(_, ref variants) => {
            let variants = compile_variants(program, path, variants);
            program.define_union(path.clone(), variants);

            Type::path(path.clone())
        }
        binary::Type::Struct(_, ref fields) => {
            let fields = compile_fields(program, path, fields);
            program.define_struct(path.clone(), fields);

            Type::path(path.clone())
        }
        binary::Type::Assert(_, ref ty, ref pred_expr) => {
            let inner_path = path.append_child("Inner");
            let inner_ty = compile_ty(program, &inner_path, ty);

            Type::Assert(Rc::new(inner_ty), pred_expr.clone())
        }
        binary::Type::Interp(_, ref ty, ref conv_expr, ref conv_ty) => {
            let inner_path = path.append_child("Inner");
            let inner_ty = compile_ty(program, &inner_path, ty);

            Type::Interp(Rc::new(inner_ty), conv_expr.clone(), conv_ty.clone())
        }
        binary::Type::Abs(_, _, _) => unimplemented!(),
        binary::Type::App(_, _, _) => unimplemented!(),
    }
}
