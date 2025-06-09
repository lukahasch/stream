use crate::Spanned;
use std::{collections::HashMap, hash::Hash, ops::Deref};

pub enum Parent<'a, T>
where
    T: Type,
{
    Root,
    Compartment(&'a mut TypeContext<'a, T>),
    Child(&'a TypeContext<'a, T>),
}

/// This trait is used for unifying types.
///
/// The methods are always supposed to transform self into a new type that satisfies the new constraint.
/// If this is not possible, an error is returned.
///
/// If a constraint has already been applied it should be ignored.
pub trait Type
where
    Self: Sized + Clone,
{
    type Error;
    type Variable: Clone + PartialEq + Hash + Eq;
    type Span: Clone + PartialEq + Eq;

    fn unknown(span: Self::Span) -> Self;

    fn eqt(self, other: Self, span: Self::Span) -> Result<Self, Self::Error>;
    fn eqv(self, v: Self::Variable, span: Self::Span) -> Result<Self, Self::Error>;

    fn subtype(self, super_type: Self, span: Self::Span) -> Result<Self, Self::Error>;
    fn subvar(self, super_var: Self::Variable, span: Self::Span) -> Result<Self, Self::Error>;

    fn supertype(self, sub_type: Self, span: Self::Span) -> Result<Self, Self::Error>;
    fn supervar(self, sub_var: Self::Variable, span: Self::Span) -> Result<Self, Self::Error>;
}

pub struct TypeContext<'a, T>
where
    T: Type,
{
    parent: Parent<'a, T>,
    variables: HashMap<T::Variable, T>,
    constraints: Vec<Spanned<Constraint<T>, T::Span>>,
}

pub enum Constraint<T>
where
    T: Type,
{
    Eqt(T::Variable, T),
    Eqv(T::Variable, T::Variable),
}

pub struct SpannedTypeContext<'a, T>
where
    T: Type,
{
    pub context: &'a mut TypeContext<'a, T>,
    pub span: T::Span,
}

impl<'a, T> TypeContext<'a, T>
where
    T: Type,
{
    pub fn new() -> Self {
        TypeContext {
            parent: Parent::Root,
            variables: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    pub fn at<'b: 'a>(&'b mut self, span: T::Span) -> SpannedTypeContext<'b, T> {
        SpannedTypeContext {
            context: self,
            span,
        }
    }
}

impl<'a, T> SpannedTypeContext<'a, T>
where
    T: Type,
{
    pub fn declare(&mut self, variable: &T::Variable) -> &mut Self {
        self.context
            .variables
            .insert(variable.clone(), T::unknown(self.span.clone()));
        self
    }

    pub fn eqt(&mut self, variable: &T::Variable, other: T) -> &mut Self {
        let constraint = Constraint::Eqt(variable.clone(), other);
        self.context
            .constraints
            .push(Spanned::new(constraint, self.span.clone()));
        self
    }

    pub fn eqv(&mut self, variable: &T::Variable, other: &T::Variable) -> &mut Self {
        let constraint = Constraint::Eqv(variable.clone(), other.clone());
        self.context
            .constraints
            .push(Spanned::new(constraint, self.span.clone()));
        self
    }
}

/*

let mut ctx = TypeContext::new();

let x = "x"
let y = "y"

ctx.at(&Span { .. }).declare(&y).eqt(&y, Type::Int);
ctx.at(&Span { .. }).declare(&x).eqv(&x, &y)

ctx.at(&Span { .. }).query(&x) == Ok(Type::Int)

T::unknown()
T::is(T::Int)

ctx.child();

Should a child be able to change the type of a variable declared in the parent context?
-> yes

very aggresive type system always trying to error out
-> enforces typing discipline

let mut c = 5;

if a == 4 {
    c = None;
}

*/
