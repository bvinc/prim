use crate::{ParseError, Type};
use prim_tok::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum NumericSuffix {
    Explicit(Type),
    None,
}

impl NumericSuffix {
    pub fn ty(&self) -> Option<&Type> {
        match self {
            NumericSuffix::Explicit(ty) => Some(ty),
            NumericSuffix::None => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub raw: Span,
    pub digits: String,
    pub suffix: NumericSuffix,
    pub value: u128,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub raw: Span,
    pub suffix: NumericSuffix,
    pub value: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedNumber {
    Int(IntegerLiteral),
    Float(FloatLiteral),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerValue {
    unsigned: u128,
}

impl IntegerValue {
    pub fn new(unsigned: u128) -> Self {
        Self { unsigned }
    }

    pub fn as_u128(&self) -> u128 {
        self.unsigned
    }

    pub fn as_i128(&self) -> i128 {
        self.unsigned as i128
    }

    pub fn as_unsigned_bits(&self, width: u8) -> u128 {
        if width >= 128 {
            return self.unsigned;
        }
        if width == 0 {
            return 0;
        }
        let mask = (1u128 << width) - 1;
        self.unsigned & mask
    }

    pub fn as_signed_bits(&self, width: u8) -> i128 {
        if width == 0 {
            return 0;
        }
        let unsigned = self.as_unsigned_bits(width);
        if width >= 128 {
            unsigned as i128
        } else {
            let sign_bit = 1u128 << (width - 1);
            if unsigned & sign_bit == 0 {
                unsigned as i128
            } else {
                let mask = (1u128 << width) - 1;
                let magnitude = ((!unsigned & mask) + 1) & mask;
                -(magnitude as i128)
            }
        }
    }

    pub fn as_bits(&self, width: u8, signed: bool) -> i128 {
        if signed {
            self.as_signed_bits(width)
        } else {
            self.as_unsigned_bits(width) as i128
        }
    }
}

pub fn parse_numeric_literal(span: Span, source: &str) -> Result<ParsedNumber, ParseError> {
    let raw_text = span.text(source);
    if raw_text.is_empty() {
        return Err(ParseError::InvalidNumericLiteral {
            span,
            message: "empty numeric literal".to_string(),
        });
    }

    let split_idx = raw_text
        .char_indices()
        .find(|(_, c)| c.is_ascii_alphabetic())
        .map(|(idx, _)| idx)
        .unwrap_or(raw_text.len());

    let numeric_part = &raw_text[..split_idx];
    let suffix_part = &raw_text[split_idx..];

    if !numeric_part.chars().any(|c| c.is_ascii_digit()) {
        return Err(ParseError::InvalidNumericLiteral {
            span,
            message: format!("numeric literal '{}' has no digits", raw_text),
        });
    }

    let suffix = parse_suffix(suffix_part, span)?;
    let suffix_ty = suffix.ty().cloned();

    let has_decimal = numeric_part.contains('.');
    let treat_as_float = has_decimal || matches!(suffix_ty, Some(Type::F32 | Type::F64));

    if treat_as_float {
        if let Some(explicit_ty) = &suffix_ty {
            if !matches!(explicit_ty, Type::F32 | Type::F64) {
                return Err(ParseError::InvalidNumericLiteral {
                    span,
                    message: format!(
                        "float literal '{}' cannot have {:?} suffix",
                        raw_text, explicit_ty
                    ),
                });
            }
        }

        let numeric_value =
            numeric_part
                .parse::<f64>()
                .map_err(|err| ParseError::InvalidNumericLiteral {
                    span,
                    message: format!("invalid float literal '{}': {}", raw_text, err),
                })?;

        return Ok(ParsedNumber::Float(FloatLiteral {
            raw: span,
            suffix,
            value: numeric_value,
        }));
    }

    if let Some(explicit_ty) = &suffix_ty {
        if matches!(explicit_ty, Type::F32 | Type::F64) {
            return Err(ParseError::InvalidNumericLiteral {
                span,
                message: format!(
                    "integer literal '{}' cannot have {:?} suffix",
                    raw_text, explicit_ty
                ),
            });
        }
    }

    let digits = numeric_part.to_string();
    let numeric_value =
        digits
            .parse::<u128>()
            .map_err(|err| ParseError::InvalidNumericLiteral {
                span,
                message: format!("invalid integer literal '{}': {}", raw_text, err),
            })?;

    Ok(ParsedNumber::Int(IntegerLiteral {
        raw: span,
        digits,
        suffix,
        value: numeric_value,
    }))
}

fn parse_suffix(text: &str, span: Span) -> Result<NumericSuffix, ParseError> {
    if text.is_empty() {
        return Ok(NumericSuffix::None);
    }

    let ty = match text {
        "u8" => Type::U8,
        "i8" => Type::I8,
        "u16" => Type::U16,
        "i16" => Type::I16,
        "u32" => Type::U32,
        "i32" => Type::I32,
        "u64" => Type::U64,
        "i64" => Type::I64,
        "usize" => Type::Usize,
        "isize" => Type::Isize,
        "f32" => Type::F32,
        "f64" => Type::F64,
        _ => {
            return Err(ParseError::InvalidNumericLiteral {
                span,
                message: format!("invalid numeric suffix '{}'", text),
            });
        }
    };

    Ok(NumericSuffix::Explicit(ty))
}

fn integer_type_bits(target: &Type) -> Option<(u8, bool)> {
    match target {
        Type::U8 => Some((8, false)),
        Type::I8 => Some((8, true)),
        Type::U16 => Some((16, false)),
        Type::I16 => Some((16, true)),
        Type::U32 => Some((32, false)),
        Type::I32 => Some((32, true)),
        Type::U64 => Some((64, false)),
        Type::I64 => Some((64, true)),
        Type::Usize => Some((usize::BITS as u8, false)),
        Type::Isize => Some((usize::BITS as u8, true)),
        _ => None,
    }
}

impl IntegerLiteral {
    pub fn span(&self) -> Span {
        self.raw
    }

    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        self.raw.text(source)
    }

    pub fn constrain_to(&self, target: &Type) -> Result<IntegerValue, crate::TypeCheckError> {
        use crate::TypeCheckError;

        let Some((bits, signed)) = integer_type_bits(target) else {
            return Err(TypeCheckError::NumericLiteralOutOfRange {
                literal: self.raw,
                target: target.clone(),
            });
        };

        if signed {
            if bits == 0 {
                return Err(TypeCheckError::NumericLiteralOutOfRange {
                    literal: self.raw,
                    target: target.clone(),
                });
            }
            let max = (1u128 << (bits - 1)) - 1;
            if self.value > max {
                return Err(TypeCheckError::NumericLiteralOutOfRange {
                    literal: self.raw,
                    target: target.clone(),
                });
            }
        } else {
            let max = if bits >= 128 {
                u128::MAX
            } else {
                (1u128 << bits) - 1
            };
            if self.value > max {
                return Err(TypeCheckError::NumericLiteralOutOfRange {
                    literal: self.raw,
                    target: target.clone(),
                });
            }
        }

        Ok(IntegerValue::new(self.value))
    }
}

impl FloatLiteral {
    pub fn span(&self) -> Span {
        self.raw
    }

    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        self.raw.text(source)
    }

    pub fn as_f32(&self) -> Result<f32, crate::TypeCheckError> {
        use crate::TypeCheckError;

        if !self.value.is_finite() || self.value < f32::MIN as f64 || self.value > f32::MAX as f64 {
            return Err(TypeCheckError::FloatLiteralOutOfRange {
                literal: self.raw,
                target: Type::F32,
            });
        }
        Ok(self.value as f32)
    }

    pub fn as_f64(&self) -> Result<f64, crate::TypeCheckError> {
        use crate::TypeCheckError;

        if !self.value.is_finite() {
            return Err(TypeCheckError::FloatLiteralOutOfRange {
                literal: self.raw,
                target: Type::F64,
            });
        }

        Ok(self.value)
    }
}

#[cfg(test)]
impl IntegerLiteral {
    pub fn new_testing(raw: Span, digits: &str, suffix: NumericSuffix, value: u128) -> Self {
        Self {
            raw,
            digits: digits.to_string(),
            suffix,
            value,
        }
    }
}

#[cfg(test)]
impl FloatLiteral {
    pub fn new_testing(raw: Span, suffix: NumericSuffix, value: f64) -> Self {
        Self { raw, suffix, value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> ParsedNumber {
        let span = Span::new(0, src.len());
        parse_numeric_literal(span, src).unwrap()
    }

    #[test]
    fn parses_integer_with_suffix() {
        let ParsedNumber::Int(int) = parse("42u8") else {
            panic!("expected integer literal");
        };
        assert_eq!(int.digits, "42");
        assert!(matches!(int.suffix, NumericSuffix::Explicit(Type::U8)));
        assert_eq!(int.value, 42);
    }

    #[test]
    fn parses_float_without_suffix() {
        let ParsedNumber::Float(float) = parse("3.14") else {
            panic!("expected float literal");
        };
        assert!(float.suffix.ty().is_none());
        assert_eq!(float.value, 3.14);
    }

    #[test]
    fn rejects_invalid_suffix() {
        let span = Span::new(0, 3);
        let err = parse_numeric_literal(span, "1zz").unwrap_err();
        match err {
            ParseError::InvalidNumericLiteral { message, .. } => {
                assert!(message.contains("suffix"))
            }
            other => panic!("expected invalid suffix error, got {:?}", other),
        }
    }
}
