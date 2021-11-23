/// The type used for the number of faces on [Dice].
pub type DSize = usize;

pub(crate) type DefaultRng = rand::prelude::ThreadRng;


#[cfg(test)]
pub(crate) mod test_utils {
    use rand::SeedableRng;
}
