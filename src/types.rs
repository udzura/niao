#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Basic { name: String },
    Array { contained: Box<Type> },
    Tuple { members: Vec<Type> },
    NamedTuple { name: String, members: Vec<Type> },
    Pointer { contained: Box<Type> },
    Unknown,
}

impl Type {
    pub fn basic(name: String) -> Self {
        Self::Basic { name }
    }

    pub fn array(cont: Type) -> Self {
        Self::Array {
            contained: Box::new(cont),
        }
    }

    pub fn tuple(members: Vec<Type>) -> Self {
        Self::Tuple { members }
    }

    pub fn namedtuple(name: String, members: Vec<Type>) -> Self {
        Self::NamedTuple { name, members }
    }

    pub fn pointer(cont: Type) -> Self {
        Self::Pointer {
            contained: Box::new(cont),
        }
    }
}
