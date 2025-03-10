use super::expression::ExprRef;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct SymbolTable(Vec<HashMap<String, ExprRef>>);
const SYM_DEBUG: bool = true;

impl SymbolTable {
    pub fn current_frame(&self) -> usize {
        self.0.len()
    }
    pub fn default() -> Self {
        SymbolTable(vec![HashMap::new()])
    }

    pub fn get(&self, frame: usize, name: &str) -> Option<&ExprRef> {
        self.0[frame].get(name)
    }

    pub fn set(&mut self, frame: usize, name: &str, value: ExprRef) {
        if self.0.len() <= frame {
            // This should never happen in a properly designed parser.
            panic!(
                "Symbol scope not created. Internal error adding {} on frame {}",
                name, frame
            );
        }
        if SYM_DEBUG {
            println!("Symbol '{}' assigned '{:?}'", name, value);
        }
        self.0[frame].insert(name.to_string(), value);
    }

    pub fn enter_frame(&mut self) -> usize {
        self.0.push(HashMap::new());
        self.0.len() - 1
    }

    pub fn exit_frame(&mut self) -> usize {
        self.0.pop();
        self.0.len() - 1
    }
}
