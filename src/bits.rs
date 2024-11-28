#[inline]
pub(crate) fn mask_up_to_lowest_bit_set(x: u64) -> u64 {
    x ^ x.wrapping_sub(1)
}

#[inline]
pub(crate) fn extract_lowest_bit_set(x: u64) -> u64 {
    x & x.wrapping_neg()
}

#[inline]
pub(crate) fn reset_lowest_bit_set(x: u64) -> u64 {
    debug_assert!(x != 0);
    x & x.wrapping_sub(1)
}

#[inline]
pub(crate) fn fill_ones_below_lowest_bit_set(x: u64) -> u64 {
    debug_assert!(x != 0);
    x | x.wrapping_sub(1)
}

#[inline]
pub(crate) fn extract_highest_bit_set(x: u64) -> u64 {
    debug_assert!(x != 0);
    1 << (63 - x.leading_zeros())
}

#[inline]
pub(crate) fn reset_highest_bit_set(x: u64) -> u64 {
    debug_assert!(x != 0);
    x & !extract_highest_bit_set(x)
}

#[inline]
pub(crate) fn mask_from_highest_bit_set(x: u64) -> u64 {
    mask_up_to_lowest_bit_set(x.reverse_bits()).reverse_bits()
}
