#[derive(Debug, Clone, PartialEq)]
pub enum LangType {
    Integer,
    Float,
    String,
    Boolean,
    Named {
        name: String,
        parent: Option<Box<LangType>>,
    },
    Unit,
    Unresolved,
}

impl LangType {
    pub fn scalar(&self) -> bool {
        match self {
            LangType::Integer | LangType::Float => true,
            _ => false,
        }
    }

    pub fn comparable(lhs_t: LangType, rhs_t: LangType) -> bool {
        if lhs_t.scalar() && rhs_t.scalar() {
            return true;
        }

        match (lhs_t, rhs_t) {
            (LangType::Boolean, LangType::Boolean) => true,
            (LangType::String, LangType::String) => true,
            _ => false,
        }
    }

    pub fn both_string(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (&Self::String, &Self::String) => true,
            _ => false,
        }
    }

    pub fn both_boolean(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (&Self::Boolean, &Self::Boolean) => true,
            _ => false,
        }
    }

    pub fn both_scalar(lhs: &Self, rhs: &Self) -> bool {
        lhs.scalar() && rhs.scalar()
    }

    // Exact type match including user defined types
    pub fn same_as(&self, other: &Self) -> bool {
        if let (LangType::Named { name: name1, .. }, LangType::Named { name: name2, .. }) =
            (self, other)
        {
            name1 == name2
        } else {
            self.same_major_type_as(other)
        }
    }

    pub fn same_major_type_as(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    // strings can be combined or scalars can be combined, and integers with even one float part
    // become floats.
    pub fn type_of_expression_parts(lhs_type: &Self, rhs_type: &Self) -> Self {
        if LangType::both_scalar(&lhs_type, &rhs_type) {
            if matches!(lhs_type, LangType::Float) || matches!(rhs_type, LangType::Float) {
                LangType::Float
            } else {
                LangType::Integer
            }
        } else if LangType::both_string(&lhs_type, &rhs_type) {
            LangType::String
        } else if LangType::both_boolean(&lhs_type, &rhs_type) {
            LangType::Boolean
        } else {
            panic!(
                "Invalid type combination '{:?}', '{:?}'",
                lhs_type, rhs_type
            );
        }
    }
}

use std::fmt;

impl fmt::Display for LangType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Integer => write!(f, "integer"),
            Self::Float => write!(f, "float"),
            Self::Boolean => write!(f, "boolean"),
            Self::String => write!(f, "string"),
            Self::Unit => write!(f, "unit"),
            Self::Unresolved => write!(f, "UNRESOLVED"),
            Self::Named { name, parent } => {
                if let Some(t) = parent {
                    write!(f, "{} ({})", name, t)
                } else {
                    write!(f, "{}", name)
                }
            }
        } // match
    } // fmt
}
