extern crate tiny_keccak;
use tiny_keccak::{Hasher, Keccak, Sha3};

#[no_mangle]
pub extern "C" fn sha3_256(data: *const u8, len: usize, out: *mut u8) {
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let mut hasher = Sha3::v256();
    hasher.update(slice);
    unsafe {
        hasher.finalize(std::slice::from_raw_parts_mut(out, 32));
    }
}

#[no_mangle]
pub extern "C" fn keccak_256(data: *const u8, len: usize, out: *mut u8) {
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let mut keccak = Keccak::v256();
    keccak.update(slice);
    unsafe {
        keccak.finalize(std::slice::from_raw_parts_mut(out, 32));
    }
}
