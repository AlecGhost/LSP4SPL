#![allow(dead_code)]
pub mod ast;
pub mod parser;
pub mod table;

pub trait DiagnosticsBroker<E> {
    fn report_error(&self, error: E);
}

pub(crate) mod test {
    use std::{rc::Rc, cell::RefCell};
    use crate::DiagnosticsBroker;

    #[derive(Debug)]
    pub(crate) struct LocalBroker<E>(Rc<RefCell<Vec<E>>>);

    impl<E> Clone for LocalBroker<E> {
        fn clone(&self) -> Self {
            LocalBroker(Rc::clone(&self.0))
        }
    }

    impl<E: Clone> LocalBroker<E> {
        pub fn new() -> Self {
            Self(Rc::new(RefCell::new(Vec::new())))
        }

        pub fn errors(&self) -> Vec<E> {
            self.0.borrow().clone()
        }
    }

    impl<E> DiagnosticsBroker<E> for LocalBroker<E> {
        fn report_error(&self, error: E) {
            self.0.borrow_mut().push(error);
        }
    }
}
