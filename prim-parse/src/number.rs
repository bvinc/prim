use crate::{ParseError, Span, Type};

fn parse_int_suffix(suffix: &str, span: Span, literal: &str) -> Result<Type, ParseError> {
    if suffix.is_empty() {
        return Ok(Type::Undetermined);
    }
    let suffix = suffix.strip_prefix('_').unwrap_or(suffix);
    let first = suffix.chars().next().unwrap_or(' ');
    if first == 'e' || first == 'E' {
        return Err(ParseError::InvalidIntegerLiteral {
            literal: literal.to_string(),
            position: span.start(),
        });
    }
    let ty = match suffix {
        "i8" => Type::I8,
        "i16" => Type::I16,
        "i32" => Type::I32,
        "i64" => Type::I64,
        "isize" => Type::Isize,
        "u8" => Type::U8,
        "u16" => Type::U16,
        "u32" => Type::U32,
        "u64" => Type::U64,
        "usize" => Type::Usize,
        _ => {
            return Err(ParseError::InvalidIntegerLiteral {
                literal: literal.to_string(),
                position: span.start(),
            });
        }
    };
    Ok(ty)
}

fn parse_float_suffix(suffix: &str, span: Span, literal: &str) -> Result<Type, ParseError> {
    if suffix.is_empty() {
        return Ok(Type::Undetermined);
    }
    let suffix = suffix.strip_prefix('_').unwrap_or(suffix);
    let ty = match suffix {
        "f32" => Type::F32,
        "f64" => Type::F64,
        _ => {
            return Err(ParseError::InvalidFloatLiteral {
                literal: literal.to_string(),
                position: span.start(),
            });
        }
    };
    Ok(ty)
}

fn split_literal(literal: &str, mut is_digit: impl FnMut(char) -> bool) -> (&str, &str) {
    let mut idx = 0usize;
    let mut iter = literal.char_indices().peekable();
    while let Some((i, ch)) = iter.next() {
        if is_digit(ch) {
            idx = i + ch.len_utf8();
            continue;
        }
        if ch == '_' {
            if iter
                .peek()
                .is_some_and(|(_, next)| next.is_ascii_alphabetic())
            {
                break;
            }
            idx = i + ch.len_utf8();
            continue;
        }
        break;
    }
    literal.split_at(idx)
}

pub fn parse_int_literal(literal: &str, span: Span) -> Result<(i64, Type), ParseError> {
    let (radix, digits) = if let Some(rest) = literal.strip_prefix("0b") {
        (2, rest)
    } else if let Some(rest) = literal.strip_prefix("0o") {
        (8, rest)
    } else if let Some(rest) = literal.strip_prefix("0x") {
        (16, rest)
    } else {
        (10, literal)
    };

    let (digit_part, suffix) = match radix {
        2 => split_literal(digits, |c| c == '0' || c == '1'),
        8 => split_literal(digits, |c| c.is_ascii_digit() && c < '8'),
        16 => split_literal(digits, |c| c.is_ascii_hexdigit()),
        _ => split_literal(digits, |c| c.is_ascii_digit()),
    };

    if digit_part.is_empty() {
        return Err(ParseError::InvalidIntegerLiteral {
            literal: literal.to_string(),
            position: span.start(),
        });
    }

    let suffix = suffix.strip_prefix('_').unwrap_or(suffix);
    if !suffix.is_empty() && !suffix.chars().next().unwrap_or(' ').is_ascii_alphabetic() {
        return Err(ParseError::InvalidIntegerLiteral {
            literal: literal.to_string(),
            position: span.start(),
        });
    }

    let ty = parse_int_suffix(suffix, span, literal)?;
    let digit_part = digit_part.replace('_', "");
    let value =
        i64::from_str_radix(&digit_part, radix).map_err(|_| ParseError::InvalidIntegerLiteral {
            literal: literal.to_string(),
            position: span.start(),
        })?;
    Ok((value, ty))
}

pub fn parse_float_literal(literal: &str, span: Span) -> Result<(f64, Type), ParseError> {
    let mut idx = 0usize;
    let mut saw_digit = false;
    let mut saw_dot = false;
    let mut saw_exp = false;
    let mut iter = literal.char_indices().peekable();
    while let Some((i, ch)) = iter.next() {
        if ch.is_ascii_digit() {
            saw_digit = true;
            idx = i + ch.len_utf8();
            continue;
        }
        if ch == '_' {
            if iter
                .peek()
                .is_some_and(|(_, next)| next.is_ascii_alphabetic())
            {
                break;
            }
            idx = i + ch.len_utf8();
            continue;
        }
        if ch == '.' && !saw_dot && !saw_exp {
            saw_dot = true;
            idx = i + ch.len_utf8();
            continue;
        }
        if (ch == 'e' || ch == 'E') && !saw_exp {
            saw_exp = true;
            idx = i + ch.len_utf8();
            if let Some((j, sign)) = iter.peek().copied() {
                if sign == '+' || sign == '-' {
                    iter.next();
                    idx = j + sign.len_utf8();
                }
            }
            continue;
        }
        break;
    }
    let (num_part, suffix) = literal.split_at(idx);
    if num_part.is_empty() || !saw_digit {
        return Err(ParseError::InvalidFloatLiteral {
            literal: literal.to_string(),
            position: span.start(),
        });
    }
    let suffix = suffix.strip_prefix('_').unwrap_or(suffix);
    if !suffix.is_empty() && !suffix.chars().next().unwrap_or(' ').is_ascii_alphabetic() {
        return Err(ParseError::InvalidFloatLiteral {
            literal: literal.to_string(),
            position: span.start(),
        });
    }
    let ty = parse_float_suffix(suffix, span, literal)?;
    let num_part = num_part.replace('_', "");
    let value = num_part
        .parse::<f64>()
        .map_err(|_| ParseError::InvalidFloatLiteral {
            literal: literal.to_string(),
            position: span.start(),
        })?;
    Ok((value, ty))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_int_with_prefix_and_suffix() {
        let span = Span::new(0, 0);
        let (value, ty) = parse_int_literal("0xFFu8", span).unwrap();
        assert_eq!(value, 255);
        assert_eq!(ty, Type::U8);
    }

    #[test]
    fn parse_int_rejects_e_suffix() {
        let span = Span::new(0, 0);
        let err = parse_int_literal("10e3", span).unwrap_err();
        assert!(matches!(err, ParseError::InvalidIntegerLiteral { .. }));
    }

    #[test]
    fn parse_float_with_suffix() {
        let span = Span::new(0, 0);
        let (value, ty) = parse_float_literal("3.5f32", span).unwrap();
        assert_eq!(value, 3.5);
        assert_eq!(ty, Type::F32);
    }

    #[test]
    fn parse_float_with_exponent_and_suffix() {
        let span = Span::new(0, 0);
        let (value, ty) = parse_float_literal("12E+2_f64", span).unwrap();
        assert_eq!(value, 1200.0);
        assert_eq!(ty, Type::F64);
    }

    #[test]
    fn parse_float_trailing_dot() {
        let span = Span::new(0, 0);
        let (value, ty) = parse_float_literal("2.", span).unwrap();
        assert_eq!(value, 2.0);
        assert_eq!(ty, Type::Undetermined);
    }
}
