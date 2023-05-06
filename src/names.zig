//! Enums naming things

/// 8086 register names. the order of the values in this enum has been chosen to match
/// table 4-9 in the 8086 users manual (page 263). the reg field is combined with the w
/// bit to make a number that maps to an enum value. see bitsToReg.
pub const Register = enum(u8) {
    // zig fmt: off
    al, cl, dl, bl, // a, b, c, d low. (byte)
    ah, ch, dh, bh, // a, b, c, d high. (byte)
    ax, cx, dx, bx, // a, b, c, d wide. (word, aka two bytes)
    sp, bp, si, di, // stack pointer, base pointer, source and destination. (word)
    es, cs, ss, ds, // segment registers
    // zig fmt: on
};

/// Assembly mnemonic. These should match the assembly language exactly, as the printing
/// logic uses @tagName to get the strings at print time.
pub const Mnemonic = enum {
    mov,

    add,
    adc,
    inc,

    sub,
    sbb,
    dec,

    @"and",
    @"or",
    xor,
    cmp,
    @"test",

    mul,
    imul,
    div,
    idiv,
    neg,
    not,
    shl,
    shr,
    sar,
    rol,
    ror,
    rcl,
    rcr,

    aaa,
    daa,
    aas,
    das,
    aam,
    aad,

    cbw,
    cwd,

    call,
    @"call far",
    jmp,
    @"jmp far",
    ret,
    retf,
    je,
    jl,
    jle,
    jb,
    jbe,
    jp,
    jo,
    js,
    jne,
    jnl,
    jg,
    jnb,
    jnbe,
    jnp,
    jno,
    jns,
    loop,
    loopz,
    loopnz,
    jcxz,

    push,
    pop,
    pushf,
    popf,

    xchg,

    in,
    out,

    xlat,

    lea,
    lds,
    les,
    lahf,
    sahf,

    movsb,
    cmpsb,
    scasb,
    lodsb,
    stosb,
    movsw,
    cmpsw,
    scasw,
    lodsw,
    stosw,

    int,
    int3,
    into,
    iret,
    clc,
    cmc,
    stc,
    cld,
    std,
    cli,
    sti,
    hlt,
    wait,

    unknown,
};
