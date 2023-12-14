use crate::ast::{DeclarePattern, Type};

pub fn param_permits_arg(pattern: &DeclarePattern, ty: &Type) -> bool {
    match pattern {
        DeclarePattern::List { .. } => {
            if !(&Type::List(Type::Any.into()) >= ty) {
                // println!("[{name:?}] does not match sig: list");
                return false;
            }
        }
        DeclarePattern::Tuple { .. } => {
            if !(&Type::Tuple(None) >= ty) {
                // println!("[{name:?}] does not match sig: tuple");
                return false;
            }
        }
        DeclarePattern::Id(_, Some(param_type)) => {
            if !(param_type >= ty) {
                // println!(
                //     "[{name:?}] does not match sig: type, because NOT {} >= {}",
                //     param_type,
                //     ty
                // );
                return false;
            }
        }
        _ => {}
    }

    true
}
