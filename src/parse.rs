use proc_macro2::Span;
use syn::{
    braced, bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    token::{Brace, Bracket, Paren},
    Error, Ident, LitFloat, LitInt, LitStr, Result, Token,
};

mod kw {
    syn::custom_keyword!(sch);
    syn::custom_keyword!(ver);
    syn::custom_keyword!(add);
    syn::custom_keyword!(rem);
    syn::custom_keyword!(obj);
    syn::custom_keyword!(cmd);
    syn::custom_keyword!(any);
}

#[derive(Debug, PartialEq)]
pub struct Scheme {
    name: String,
    uses: Vec<Use>,
    types: Vec<Type>,
}

impl Parse for Scheme {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = {
            let _: kw::sch = input.parse()?;
            let name: LitStr = input.parse()?;
            let _: Token![;] = input.parse()?;
            name.value()
        };

        let mut uses = Vec::new();
        let mut types = Vec::new();
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![use]) {
                let _: Token![use] = input.parse()?;
                uses.push(Use::parse(input)?);
                continue;
            }

            if !lookahead.peek(Token![@]) {
                return Err(lookahead.error());
            }

            let _: Token![@] = input.parse()?;
            let _: kw::ver = input.parse()?;
            let version = MajorVersion::parse(input)?;

            let type_def = {
                let lookahead = input.lookahead1();
                if lookahead.peek(kw::obj) {
                    let _: kw::obj = input.parse()?;
                    let object = Object::parse(input, version)?;
                    Type::Object(object)
                } else if lookahead.peek(Token![struct]) {
                    let _: Token![struct] = input.parse()?;
                    let struct_ = Struct::parse(input, version)?;
                    Type::Struct(struct_)
                } else if lookahead.peek(Token![union]) {
                    let _: Token![union] = input.parse()?;
                    let union_ = Union::parse(input, version)?;
                    Type::Union(union_)
                } else if lookahead.peek(Token![enum]) {
                    let _: Token![enum] = input.parse()?;
                    let enum_ = Enum::parse(input, version)?;
                    Type::Enum(enum_)
                } else if lookahead.peek(Token![fn]) {
                    let _: Token![fn] = input.parse()?;
                    let fn_ = Function::parse(input, version)?;
                    Type::Function(fn_)
                } else if lookahead.peek(kw::cmd) {
                    let _: kw::cmd = input.parse()?;
                    let cmd_ = Command::parse(input, version)?;
                    Type::Command(cmd_)
                } else {
                    return Err(lookahead.error());
                }
            };

            types.push(type_def);
        }

        Ok(Self { name, uses, types })
    }
}

#[derive(Debug, PartialEq)]
pub struct Use {
    segments: Vec<String>,
    alias: Option<String>,
}

impl Use {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut segments = Vec::new();
        let mut alias = None;

        segments.push(input.call(Ident::parse_any)?.to_string());

        if input.is_empty() {
            return Err(input.error("expected ';', or '::identifier'"));
        }

        while !input.is_empty() {
            if input.peek(Token![;]) {
                let _: Token![;] = input.parse()?;
                break;
            } else if input.peek(Token![as]) {
                let _: Token![as] = input.parse()?;
                alias = Some(input.parse::<Ident>()?.to_string());
                let _: Token![;] = input.parse()?;
                break;
            }

            let _: Token![:] = input.parse()?;
            let _: Token![:] = input.parse()?;

            segments.push(input.parse::<Ident>()?.to_string());
        }

        Ok(Self { segments, alias })
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Object(Object),
    Struct(Struct),
    Union(Union),
    Enum(Enum),
    Function(Function),
    Command(Command),
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Struct(Struct),
    Union(Union),
    Enum(Enum),
}

impl Object {
    fn parse(input: ParseStream, version: MajorVersion) -> Result<Self> {
        let kind_input;
        parenthesized!(kind_input in input);

        let lookahead = kind_input.lookahead1();
        if lookahead.peek(Token![struct]) {
            Ok(Object::Struct(Struct::parse(input, version)?))
        } else if lookahead.peek(Token![union]) {
            Ok(Object::Union(Union::parse(input, version)?))
        } else if lookahead.peek(Token![enum]) {
            Ok(Object::Enum(Enum::parse(input, version)?))
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug)]
pub struct Struct {
    pub version: MajorVersion,
    pub name_span: Span,
    pub name: String,
    pub body: StructBody,
}

impl Struct {
    fn parse(input: ParseStream, version: MajorVersion) -> Result<Self> {
        let name_ident: Ident = input.parse()?;
        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            body: StructBody::parse(input, false)?,
        })
    }
}

impl PartialEq<Struct> for Struct {
    fn eq(&self, other: &Struct) -> bool {
        self.version == other.version && self.name == other.name && self.body == other.body
    }
}

#[derive(Debug)]
pub struct Union {
    pub version: MajorVersion,
    pub name_span: Span,
    pub name: String,
    pub items: Vec<UnionItem>,
}

impl Union {
    fn parse(input: ParseStream, version: MajorVersion) -> Result<Self> {
        let name_ident: Ident = input.parse()?;

        let items_input;
        braced!(items_input in input);
        let input = &items_input;

        let mut items = Vec::new();
        while !input.is_empty() {
            let (include, version) = parse_include_or_minor_version(input, false, "union fields")?;
            if include.is_some() {
                items.push(UnionItem::Include(include.unwrap()));
            } else {
                items.push(UnionItem::Field(UnionField::parse(input, version)?));
            }
            let _: Token![,] = input.parse()?;
        }

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            items,
        })
    }
}

impl PartialEq<Union> for Union {
    fn eq(&self, other: &Union) -> bool {
        self.version == other.version && self.name == other.name && self.items == other.items
    }
}

#[derive(Debug, PartialEq)]
pub enum UnionItem {
    Include(Include),
    Field(UnionField),
}

#[derive(Debug)]
pub struct UnionField {
    pub version: Option<MinorVersion>,
    pub name_span: Span,
    pub name: String,
    pub body: StructBody,
}

impl UnionField {
    fn parse(input: ParseStream, version: Option<MinorVersion>) -> Result<Self> {
        if !input.peek(Ident::peek_any) {
            return Err(input.error("expected union field name"));
        }

        let name_ident: Ident = input.parse()?;
        let body = StructBody::parse(input, true)?;

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            body,
        })
    }
}

impl PartialEq<UnionField> for UnionField {
    fn eq(&self, other: &UnionField) -> bool {
        self.version == other.version && self.name == other.name && self.body == other.body
    }
}

#[derive(Debug, PartialEq)]
pub enum StructBody {
    Items(Vec<StructItem>),
    Tuple(Tuple),
    Unit,
}

impl StructBody {
    fn parse(input: ParseStream, is_union_field: bool) -> Result<Self> {
        let lookahead = input.lookahead1();
        let peek_closing_char = || {
            if is_union_field {
                lookahead.peek(Token![,])
            } else {
                lookahead.peek(Token![;])
            }
        };

        if lookahead.peek(Brace) {
            let items_input;
            braced!(items_input in input);
            let items = StructItem::parse_all(&items_input, "struct fields")?;
            Ok(StructBody::Items(items))
        } else if lookahead.peek(Paren) {
            Ok(StructBody::Tuple(Tuple::parse(input)?))
        } else if peek_closing_char() {
            if input.peek(Token![;]) {
                let _: Token![;] = input.parse()?;
            }
            Ok(StructBody::Unit)
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum StructItem {
    Include(Include),
    Field(StructField),
}

impl StructItem {
    fn parse_all(input: ParseStream, invalid_for: &str) -> Result<Vec<Self>> {
        let mut items = Vec::new();

        while !input.is_empty() {
            let (include, version) = parse_include_or_minor_version(input, true, invalid_for)?;
            if include.is_some() {
                items.push(StructItem::Include(include.unwrap()));
            } else {
                items.push(StructItem::Field(StructField::parse(input, version)?));
            }
            let _: Token![,] = input.parse()?;
        }

        Ok(items)
    }
}

#[derive(Debug)]
pub struct StructField {
    pub version: Option<MinorVersion>,
    pub name_span: Span,
    pub name: String,
    pub field_type: FieldType,
}

impl StructField {
    fn parse(input: ParseStream, version: Option<MinorVersion>) -> Result<Self> {
        if !input.peek(Ident::peek_any) {
            return Err(input.error("expected struct field name"));
        }

        let name_ident: Ident = input.parse()?;
        let _: Token![:] = input.parse()?;
        let field_type = FieldType::parse(input)?;

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            field_type,
        })
    }
}

impl PartialEq<StructField> for StructField {
    fn eq(&self, other: &StructField) -> bool {
        self.version == other.version
            && self.name == other.name
            && self.field_type == other.field_type
    }
}

#[derive(Debug, PartialEq)]
pub struct Tuple(Vec<TupleItem>);

impl Tuple {
    fn parse(input: ParseStream) -> Result<Self> {
        let items_input;
        parenthesized!(items_input in input);
        let input = &items_input;

        let mut items = Vec::new();
        while !input.is_empty() {
            let (include, version) = parse_include_or_minor_version(input, true, "tuple fields")?;
            if include.is_some() {
                items.push(TupleItem::Include(include.unwrap()));
            } else {
                items.push(TupleItem::Field(TupleField {
                    version,
                    field_type: FieldType::parse(input)?,
                }));
            }

            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(Self(items))
    }
}

#[derive(Debug, PartialEq)]
pub enum TupleItem {
    Include(Include),
    Field(TupleField),
}

#[derive(Debug, PartialEq)]
pub struct TupleField {
    pub version: Option<MinorVersion>,
    pub field_type: FieldType,
}

#[derive(Debug)]
pub struct Enum {
    pub version: MajorVersion,
    pub name_span: Span,
    pub name: String,
    pub items: Vec<EnumItem>,
}

impl Enum {
    fn parse(input: ParseStream, version: MajorVersion) -> Result<Self> {
        let name_ident: Ident = input.parse()?;

        let items_input;
        braced!(items_input in input);
        let input = &items_input;

        let mut items = Vec::new();
        while !input.is_empty() {
            let (include, version) = parse_include_or_minor_version(input, false, "enum fields")?;
            if include.is_some() {
                items.push(EnumItem::Include(include.unwrap()));
            } else {
                items.push(EnumItem::Field(EnumField::parse(input, version)?));
            }
            let _: Token![,] = input.parse()?;
        }

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            items,
        })
    }
}

impl PartialEq<Enum> for Enum {
    fn eq(&self, other: &Enum) -> bool {
        self.version == other.version && self.name == other.name && self.items == other.items
    }
}

#[derive(Debug, PartialEq)]
pub enum EnumItem {
    Include(Include),
    Field(EnumField),
}

#[derive(Debug)]
pub struct EnumField {
    pub version: Option<MinorVersion>,
    pub name_span: Span,
    pub name: String,
    pub value: Option<u32>,
}

impl EnumField {
    fn parse(input: ParseStream, version: Option<MinorVersion>) -> Result<Self> {
        let name_ident: Ident = input.parse()?;

        let value = if input.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            let int_lit: LitInt = input.parse()?;
            let value = parse_int_lit(&int_lit, "invalid enum int literal")?;
            Some(value)
        } else {
            None
        };

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            value,
        })
    }
}

impl PartialEq<EnumField> for EnumField {
    fn eq(&self, other: &EnumField) -> bool {
        self.version == other.version && self.name == other.name && self.value == other.value
    }
}

#[derive(Debug)]
pub struct Function {
    pub version: MajorVersion,
    pub name_span: Span,
    pub name: String,
    pub items: Vec<StructItem>,
    pub return_type: Option<FieldType>,
}

impl Function {
    fn parse(input: ParseStream, version: MajorVersion) -> Result<Self> {
        let name_ident: Ident = input.parse()?;

        let items_input;
        parenthesized!(items_input in input);
        let items = StructItem::parse_all(&items_input, "function params")?;

        let return_type = if input.peek(Token![->]) {
            let _: Token![->] = input.parse()?;
            Some(FieldType::parse(input)?)
        } else {
            None
        };

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            items,
            return_type,
        })
    }
}

impl PartialEq<Function> for Function {
    fn eq(&self, other: &Function) -> bool {
        self.version == other.version
            && self.name == other.name
            && self.items == other.items
            && self.return_type == other.return_type
    }
}

#[derive(Debug)]
pub struct Command {
    pub version: MajorVersion,
    pub name_span: Span,
    pub name: String,
    pub items: Vec<StructItem>,
}

impl Command {
    fn parse(input: ParseStream, version: MajorVersion) -> Result<Self> {
        let name_ident: Ident = input.parse()?;

        let items_input;
        parenthesized!(items_input in input);
        let items = StructItem::parse_all(&items_input, "command params")?;

        Ok(Self {
            version,
            name_span: name_ident.span(),
            name: name_ident.to_string(),
            items,
        })
    }
}

impl PartialEq<Command> for Command {
    fn eq(&self, other: &Command) -> bool {
        self.version == other.version && self.name == other.name && self.items == other.items
    }
}

fn parse_include_or_minor_version(
    input: ParseStream,
    add_is_valid: bool,
    invalid_for: &str,
) -> Result<(Option<Include>, Option<MinorVersion>)> {
    if input.peek(Token![@]) {
        let _: Token![@] = input.parse()?;
        let lookahead = input.lookahead1();
        let peek_valid_directive = || {
            if add_is_valid {
                lookahead.peek(kw::add)
            } else {
                lookahead.peek(kw::rem)
            }
        };

        let check_invalid_directive = || {
            if add_is_valid {
                if input.peek(kw::rem) {
                    Err(input.error(format!("@rem directive is not allowed for {}", invalid_for)))
                } else {
                    Ok(())
                }
            } else {
                if input.peek(kw::add) {
                    Err(input.error(format!("@add directive is not allowed for {}", invalid_for)))
                } else {
                    Ok(())
                }
            }
        };

        if lookahead.peek(kw::ver) {
            let _: kw::ver = input.parse()?;
            Ok((Some(Include::parse(input)?), None))
        } else if peek_valid_directive() {
            if add_is_valid {
                let _: kw::add = input.parse()?;
            } else {
                let _: kw::rem = input.parse()?;
            }
            Ok((None, Some(MinorVersion::parse(input)?)))
        } else {
            check_invalid_directive()?;
            Err(lookahead.error())
        }
    } else {
        Ok((None, None))
    }
}

#[derive(Debug, PartialEq)]
pub struct Include {
    pub version: MajorVersion,
    pub items: Vec<IncludeItem>,
}

impl Include {
    fn parse(input: ParseStream) -> Result<Self> {
        let version = MajorVersion::parse(input)?;

        let mut items = Vec::new();
        if input.peek(Brace) {
            let items_input;
            braced!(items_input in input);
            let input = &items_input;

            while !input.is_empty() {
                let _: Token![@] = input.parse()?;

                let lookahead = input.lookahead1();
                if lookahead.peek(kw::add) {
                    let _: kw::add = input.parse()?;

                    let ident_input;
                    parenthesized!(ident_input in input);
                    let ident: Ident = ident_input.parse()?;

                    items.push(IncludeItem::Add(ident.to_string()));
                } else if lookahead.peek(kw::rem) {
                    let _: kw::rem = input.parse()?;

                    let ident_input;
                    parenthesized!(ident_input in input);
                    let ident: Ident = ident_input.parse()?;

                    items.push(IncludeItem::Rem(ident.to_string()));
                }

                let _: Token![,] = input.parse()?;
            }
        }

        Ok(Self { version, items })
    }
}

#[derive(Debug, PartialEq)]
pub enum IncludeItem {
    Add(String),
    Rem(String),
}

#[derive(Debug, PartialEq)]
pub struct MajorVersion(u16);

impl MajorVersion {
    fn parse(input: ParseStream) -> Result<Self> {
        let value_input;
        parenthesized!(value_input in input);
        let input = &value_input;

        let int_lit: LitInt = input.parse()?;
        let value = int_lit.to_string();
        let Ok(major) = u16::from_str_radix(&value, 10) else {
            return Err(Error::new(int_lit.span(), "major version must be valid u16 value"));
        };

        Ok(Self(major))
    }
}

#[derive(Debug, PartialEq)]
pub struct MinorVersion(u16, u16);

impl MinorVersion {
    fn parse(input: ParseStream) -> Result<Self> {
        let inner_input;
        parenthesized!(inner_input in input);
        let input = &inner_input;

        let float_lit: LitFloat = input.parse()?;
        let value = float_lit.to_string();
        let (major, minor) = value.split_once('.').unwrap();

        let parse_version_num = |num: &str| -> Result<u16> {
            match u16::from_str_radix(num, 10) {
                Ok(num) => Ok(num),
                Err(_) => Err(Error::new(
                    float_lit.span(),
                    "major and minor versions must be valid u16 values",
                )),
            }
        };
        let major = parse_version_num(major)?;
        let minor = parse_version_num(minor)?;

        Ok(Self(major, minor))
    }
}

#[derive(Debug)]
pub enum FieldType {
    Primitive(Span, Primitive),
    Type(Span, String, Option<String>, Option<MajorVersion>),
    Optional(Span, Box<FieldType>),
    Reference(Span, Box<FieldType>),
    Array(Span, Box<FieldType>, u32),
    List(Span, Box<FieldType>),
    Map(Span, Box<FieldType>, Box<FieldType>),
    Tuple(Span, Tuple),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Primitive {
    Int8,
    Int16,
    Int32,
    Int64,

    UInt8,
    UInt16,
    UInt32,
    UInt64,

    Float32,
    Float64,

    Boolean,
    String,
    Bytes,

    Any,
}

impl FieldType {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident::peek_any) {
            let span = input.span();
            let ident: Ident = input.parse()?;
            let ident = ident.to_string();
            match ident.as_str() {
                "i8" => Ok(FieldType::Primitive(span, Primitive::Int8)),
                "i16" => Ok(FieldType::Primitive(span, Primitive::Int16)),
                "i32" => Ok(FieldType::Primitive(span, Primitive::Int32)),
                "i64" => Ok(FieldType::Primitive(span, Primitive::Int64)),
                "u8" => Ok(FieldType::Primitive(span, Primitive::UInt8)),
                "u16" => Ok(FieldType::Primitive(span, Primitive::UInt16)),
                "u32" => Ok(FieldType::Primitive(span, Primitive::UInt32)),
                "u64" => Ok(FieldType::Primitive(span, Primitive::UInt64)),
                "f32" => Ok(FieldType::Primitive(span, Primitive::Float32)),
                "f64" => Ok(FieldType::Primitive(span, Primitive::Float64)),
                "bool" => Ok(FieldType::Primitive(span, Primitive::Boolean)),
                "str" => Ok(FieldType::Primitive(span, Primitive::String)),
                "bytes" => Ok(FieldType::Primitive(span, Primitive::Bytes)),
                "any" => Ok(FieldType::Primitive(span, Primitive::Any)),
                _ => {
                    let second_ident = if input.peek(Token![:]) {
                        let _: Token![:] = input.parse()?;
                        let _: Token![:] = input.parse()?;

                        let ident: Ident = input.parse()?;
                        Some(ident.to_string())
                    } else {
                        None
                    };

                    let version = if input.peek(Token![@]) {
                        let _: Token![@] = input.parse()?;
                        let _: kw::ver = input.parse()?;
                        Some(MajorVersion::parse(input)?)
                    } else {
                        None
                    };

                    Ok(Self::Type(span, ident, second_ident, version))
                }
            }
        } else if lookahead.peek(Token![?]) {
            let qm: Token![?] = input.parse()?;
            Ok(Self::Optional(qm.span, Box::new(Self::parse(input)?)))
        } else if lookahead.peek(Token![&]) {
            let amp: Token![&] = input.parse()?;
            Ok(Self::Reference(amp.span, Box::new(Self::parse(input)?)))
        } else if lookahead.peek(Bracket) {
            let bracket_span = input.span();

            let bracketed_input;
            bracketed!(bracketed_input in input);
            let input = &bracketed_input;

            let element_type = Self::parse(input)?;
            if !input.is_empty() {
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![;]) {
                    let _: Token![;] = input.parse()?;
                    let size: LitInt = input.parse()?;
                    expect_empty(input)?;

                    Ok(Self::Array(
                        bracket_span,
                        Box::new(element_type),
                        parse_int_lit(&size, "invalid array size literal")?,
                    ))
                } else if lookahead.peek(Token![:]) {
                    let key_type = element_type;

                    let _: Token![:] = input.parse()?;
                    let value_type = Self::parse(input)?;
                    expect_empty(input)?;

                    Ok(Self::Map(
                        bracket_span,
                        Box::new(key_type),
                        Box::new(value_type),
                    ))
                } else {
                    Err(lookahead.error())
                }
            } else {
                Ok(Self::List(bracket_span, Box::new(element_type)))
            }
        } else if lookahead.peek(Paren) {
            let paren_span = input.span();
            let tuple = Tuple::parse(input)?;
            Ok(Self::Tuple(paren_span, tuple))
        } else {
            Err(lookahead.error())
        }
    }
}

impl PartialEq<FieldType> for FieldType {
    fn eq(&self, other: &FieldType) -> bool {
        match self {
            Self::Primitive(_, primitive) => {
                if let Self::Primitive(_, other_primitive) = other {
                    primitive == other_primitive
                } else {
                    false
                }
            }
            Self::Type(_, ident, extra_ident, version) => {
                if let Self::Type(_, other_ident, other_extra_ident, other_version) = other {
                    ident == other_ident
                        && extra_ident == other_extra_ident
                        && version == other_version
                } else {
                    false
                }
            }
            Self::Optional(_, field_type) => {
                if let Self::Optional(_, other_field_type) = other {
                    field_type == other_field_type
                } else {
                    false
                }
            }
            Self::Reference(_, field_type) => {
                if let Self::Reference(_, other_field_type) = other {
                    field_type == other_field_type
                } else {
                    false
                }
            }
            Self::Array(_, element_type, size) => {
                if let Self::Array(_, other_element_type, other_size) = other {
                    element_type == other_element_type && size == other_size
                } else {
                    false
                }
            }
            Self::List(_, element_type) => {
                if let Self::List(_, other_element_type) = other {
                    element_type == other_element_type
                } else {
                    false
                }
            }
            Self::Map(_, key_type, value_type) => {
                if let Self::Map(_, other_key_type, other_value_type) = other {
                    key_type == other_key_type && value_type == other_value_type
                } else {
                    false
                }
            }
            Self::Tuple(_, tuple) => {
                if let Self::Tuple(_, other_tuple) = other {
                    tuple == other_tuple
                } else {
                    false
                }
            }
        }
    }
}

fn parse_int_lit(int_lit: &LitInt, msg: &str) -> Result<u32> {
    match u32::from_str_radix(&int_lit.to_string(), 10) {
        Ok(int) => Ok(int),
        Err(_) => Err(Error::new(int_lit.span(), msg)),
    }
}

fn expect_empty(input: ParseStream) -> Result<()> {
    if input.is_empty() {
        Ok(())
    } else {
        Err(input.error("expected nothing here"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn all_valid_input() {
        let scheme: Scheme = syn::parse2(quote! {
            sch "scheme/name";

            use external_crate::some_path::scheme as extern_scheme;
            use super::scheme as super_scheme;
            use crate::some_path::types;

            @ver(1)
            struct Struct {
                signed_int8: i8,
                signed_int16: i16,
                signed_int32: i32,
                signed_int64: i64,

                unsigned_int8: u8,
                unsigned_int16: u16,
                unsigned_int32: u32,
                unsigned_int64: u64,

                float32: f32,
                float64: f64,

                boolean: bool,
                string: str,
                optional: ?str,

                array: [u8; 32],
                list: [u8],
                map: [u8: u8],
                tuple: (u8, u8),
                byte_list: bytes,

                extern_struct: types::Struct@ver(1),
                extern_object: &types::Object,
            }

            @ver(2)
            struct Struct {
                @ver(1) {
                    @rem(list),
                    @rem(byte_list),
                },

                array: [u16; 32],
                map: [u16: u16],
                tuple: (u16, u16),

                extern_struct: extern_scheme::Struct@ver(2),
            }

            @ver(1)
            struct NewTypeStruct (
                Struct@ver(1),
            )

            @ver(2)
            struct NewTypeStruct (
                Struct@ver(2),
            )

            @ver(1)
            struct TupleStruct (
                types::Struct@ver(2),
                super_scheme::Enum@ver(1),

                @add(1.1)
                types::Union@ver(1),
            )

            @ver(2)
            struct TupleStruct (
                types::Struct@ver(3),
                super_scheme::Enum@ver(2),
                types::Union@ver(2),
            )

            @ver(1)
            enum Enum {
                @rem(1.1)
                Zero,

                One = 10,
                Two = 20,
                Three = 30,
            }

            @ver(2)
            enum Enum {
                One = 100,
                Two = 200,
                Three = 300,

                @rem(2.1)
                Four = 400,
            }

            @ver(1)
            union Union {
                NewType(u8),
                Tuple(
                    u8,
                    bool,

                    @add(1.1)
                    u16,
                ),
                Struct {
                    new_type: NewTypeStruct@ver(1),
                    tuple: TupleStruct@ver(1),
                    extern_type: extern_scheme::Union@ver(1),
                },

                @rem(1.2)
                None,
            }

            @ver(2)
            union Union {
                @ver(1),
                None2,
            }

            @ver(1)
            obj(struct) Object {
                struct_: Struct@ver(1),
            }

            @ver(2)
            obj(struct) Object (
                Enum@ver(1),
            )

            @ver(3)
            obj(union) Object {
                Struct {
                    extern_object: &types::Object,
                },
            }

            @ver(4)
            obj(enum) Object {
                Zero,
            }

            @ver(1)
            fn Function (
                one: Struct@ver(1),
                two: &Object,
                three: bytes,
            ) -> Struct@ver(1)

            @ver(1)
            cmd Command (
                one: &Object,
                two: &types::Object,
            )
        })
        .unwrap();
        let dummy_span = Span::call_site();
        assert_eq!(
            scheme,
            Scheme {
                name: "scheme/name".to_string(),
                uses: vec![
                    Use {
                        segments: vec![
                            "external_crate".to_string(),
                            "some_path".to_string(),
                            "scheme".to_string()
                        ],
                        alias: Some("extern_scheme".to_string()),
                    },
                    Use {
                        segments: vec!["super".to_string(), "scheme".to_string(),],
                        alias: Some("super_scheme".to_string()),
                    },
                    Use {
                        segments: vec![
                            "crate".to_string(),
                            "some_path".to_string(),
                            "types".to_string()
                        ],
                        alias: None,
                    },
                ],
                types: vec![
                    Type::Struct(Struct {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "Struct".to_string(),
                        body: StructBody::Items(vec![
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "signed_int8".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Int8),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "signed_int16".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Int16),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "signed_int32".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Int32),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "signed_int64".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Int64),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "unsigned_int8".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::UInt8),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "unsigned_int16".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::UInt16),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "unsigned_int32".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::UInt32),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "unsigned_int64".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::UInt64),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "float32".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Float32),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "float64".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Float64),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "boolean".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Boolean),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "string".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::String),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "optional".to_string(),
                                field_type: FieldType::Optional(
                                    dummy_span,
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::String))
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "array".to_string(),
                                field_type: FieldType::Array(
                                    dummy_span,
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt8)),
                                    32
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "list".to_string(),
                                field_type: FieldType::List(
                                    dummy_span,
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt8)),
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "map".to_string(),
                                field_type: FieldType::Map(
                                    dummy_span,
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt8)),
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt8)),
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "tuple".to_string(),
                                field_type: FieldType::Tuple(
                                    dummy_span,
                                    Tuple(vec![
                                        TupleItem::Field(TupleField {
                                            version: None,
                                            field_type: FieldType::Primitive(
                                                dummy_span,
                                                Primitive::UInt8
                                            ),
                                        }),
                                        TupleItem::Field(TupleField {
                                            version: None,
                                            field_type: FieldType::Primitive(
                                                dummy_span,
                                                Primitive::UInt8
                                            ),
                                        },),
                                    ]),
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "byte_list".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Bytes),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "extern_struct".to_string(),
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "types".to_string(),
                                    Some("Struct".to_string()),
                                    Some(MajorVersion(1))
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "extern_object".to_string(),
                                field_type: FieldType::Reference(
                                    dummy_span,
                                    Box::new(FieldType::Type(
                                        dummy_span,
                                        "types".to_string(),
                                        Some("Object".to_string()),
                                        None,
                                    )),
                                ),
                            }),
                        ])
                    }),
                    Type::Struct(Struct {
                        version: MajorVersion(2),
                        name_span: dummy_span,
                        name: "Struct".to_string(),
                        body: StructBody::Items(vec![
                            StructItem::Include(Include {
                                version: MajorVersion(1),
                                items: vec![
                                    IncludeItem::Rem("list".to_string()),
                                    IncludeItem::Rem("byte_list".to_string())
                                ],
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "array".to_string(),
                                field_type: FieldType::Array(
                                    dummy_span,
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt16)),
                                    32,
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "map".to_string(),
                                field_type: FieldType::Map(
                                    dummy_span,
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt16)),
                                    Box::new(FieldType::Primitive(dummy_span, Primitive::UInt16)),
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "tuple".to_string(),
                                field_type: FieldType::Tuple(
                                    dummy_span,
                                    Tuple(vec![
                                        TupleItem::Field(TupleField {
                                            version: None,
                                            field_type: FieldType::Primitive(
                                                dummy_span,
                                                Primitive::UInt16
                                            ),
                                        }),
                                        TupleItem::Field(TupleField {
                                            version: None,
                                            field_type: FieldType::Primitive(
                                                dummy_span,
                                                Primitive::UInt16
                                            ),
                                        }),
                                    ])
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "extern_struct".to_string(),
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "extern_scheme".to_string(),
                                    Some("Struct".to_string()),
                                    Some(MajorVersion(2)),
                                ),
                            }),
                        ]),
                    }),
                    Type::Struct(Struct {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "NewTypeStruct".to_string(),
                        body: StructBody::Tuple(Tuple(vec![TupleItem::Field(TupleField {
                            version: None,
                            field_type: FieldType::Type(
                                dummy_span,
                                "Struct".to_string(),
                                None,
                                Some(MajorVersion(1))
                            ),
                        }),])),
                    }),
                    Type::Struct(Struct {
                        version: MajorVersion(2),
                        name_span: dummy_span,
                        name: "NewTypeStruct".to_string(),
                        body: StructBody::Tuple(Tuple(vec![TupleItem::Field(TupleField {
                            version: None,
                            field_type: FieldType::Type(
                                dummy_span,
                                "Struct".to_string(),
                                None,
                                Some(MajorVersion(2))
                            ),
                        }),])),
                    }),
                    Type::Struct(Struct {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "TupleStruct".to_string(),
                        body: StructBody::Tuple(Tuple(vec![
                            TupleItem::Field(TupleField {
                                version: None,
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "types".to_string(),
                                    Some("Struct".to_string()),
                                    Some(MajorVersion(2))
                                ),
                            }),
                            TupleItem::Field(TupleField {
                                version: None,
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "super_scheme".to_string(),
                                    Some("Enum".to_string()),
                                    Some(MajorVersion(1))
                                ),
                            }),
                            TupleItem::Field(TupleField {
                                version: Some(MinorVersion(1, 1)),
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "types".to_string(),
                                    Some("Union".to_string()),
                                    Some(MajorVersion(1))
                                ),
                            }),
                        ])),
                    }),
                    Type::Struct(Struct {
                        version: MajorVersion(2),
                        name_span: dummy_span,
                        name: "TupleStruct".to_string(),
                        body: StructBody::Tuple(Tuple(vec![
                            TupleItem::Field(TupleField {
                                version: None,
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "types".to_string(),
                                    Some("Struct".to_string()),
                                    Some(MajorVersion(3))
                                ),
                            }),
                            TupleItem::Field(TupleField {
                                version: None,
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "super_scheme".to_string(),
                                    Some("Enum".to_string()),
                                    Some(MajorVersion(2))
                                ),
                            }),
                            TupleItem::Field(TupleField {
                                version: None,
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "types".to_string(),
                                    Some("Union".to_string()),
                                    Some(MajorVersion(2))
                                ),
                            }),
                        ])),
                    }),
                    Type::Enum(Enum {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "Enum".to_string(),
                        items: vec![
                            EnumItem::Field(EnumField {
                                version: Some(MinorVersion(1, 1)),
                                name_span: dummy_span,
                                name: "Zero".to_string(),
                                value: None,
                            }),
                            EnumItem::Field(EnumField {
                                version: None,
                                name_span: dummy_span,
                                name: "One".to_string(),
                                value: Some(10),
                            }),
                            EnumItem::Field(EnumField {
                                version: None,
                                name_span: dummy_span,
                                name: "Two".to_string(),
                                value: Some(20),
                            }),
                            EnumItem::Field(EnumField {
                                version: None,
                                name_span: dummy_span,
                                name: "Three".to_string(),
                                value: Some(30),
                            }),
                        ],
                    }),
                    Type::Enum(Enum {
                        version: MajorVersion(2),
                        name_span: dummy_span,
                        name: "Enum".to_string(),
                        items: vec![
                            EnumItem::Field(EnumField {
                                version: None,
                                name_span: dummy_span,
                                name: "One".to_string(),
                                value: Some(100),
                            }),
                            EnumItem::Field(EnumField {
                                version: None,
                                name_span: dummy_span,
                                name: "Two".to_string(),
                                value: Some(200),
                            }),
                            EnumItem::Field(EnumField {
                                version: None,
                                name_span: dummy_span,
                                name: "Three".to_string(),
                                value: Some(300),
                            }),
                            EnumItem::Field(EnumField {
                                version: Some(MinorVersion(2, 1)),
                                name_span: dummy_span,
                                name: "Four".to_string(),
                                value: Some(400),
                            }),
                        ],
                    }),
                    Type::Union(Union {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "Union".to_string(),
                        items: vec![
                            UnionItem::Field(UnionField {
                                version: None,
                                name_span: dummy_span,
                                name: "NewType".to_string(),
                                body: StructBody::Tuple(Tuple(vec![TupleItem::Field(
                                    TupleField {
                                        version: None,
                                        field_type: FieldType::Primitive(
                                            dummy_span,
                                            Primitive::UInt8
                                        ),
                                    }
                                ),]))
                            }),
                            UnionItem::Field(UnionField {
                                version: None,
                                name_span: dummy_span,
                                name: "Tuple".to_string(),
                                body: StructBody::Tuple(Tuple(vec![
                                    TupleItem::Field(TupleField {
                                        version: None,
                                        field_type: FieldType::Primitive(
                                            dummy_span,
                                            Primitive::UInt8
                                        ),
                                    }),
                                    TupleItem::Field(TupleField {
                                        version: None,
                                        field_type: FieldType::Primitive(
                                            dummy_span,
                                            Primitive::Boolean,
                                        ),
                                    }),
                                    TupleItem::Field(TupleField {
                                        version: Some(MinorVersion(1, 1)),
                                        field_type: FieldType::Primitive(
                                            dummy_span,
                                            Primitive::UInt16,
                                        ),
                                    }),
                                ]))
                            }),
                            UnionItem::Field(UnionField {
                                version: None,
                                name_span: dummy_span,
                                name: "Struct".to_string(),
                                body: StructBody::Items(vec![
                                    StructItem::Field(StructField {
                                        version: None,
                                        name_span: dummy_span,
                                        name: "new_type".to_string(),
                                        field_type: FieldType::Type(
                                            dummy_span,
                                            "NewTypeStruct".to_string(),
                                            None,
                                            Some(MajorVersion(1)),
                                        ),
                                    }),
                                    StructItem::Field(StructField {
                                        version: None,
                                        name_span: dummy_span,
                                        name: "tuple".to_string(),
                                        field_type: FieldType::Type(
                                            dummy_span,
                                            "TupleStruct".to_string(),
                                            None,
                                            Some(MajorVersion(1)),
                                        ),
                                    }),
                                    StructItem::Field(StructField {
                                        version: None,
                                        name_span: dummy_span,
                                        name: "extern_type".to_string(),
                                        field_type: FieldType::Type(
                                            dummy_span,
                                            "extern_scheme".to_string(),
                                            Some("Union".to_string()),
                                            Some(MajorVersion(1)),
                                        ),
                                    }),
                                ]),
                            }),
                            UnionItem::Field(UnionField {
                                version: Some(MinorVersion(1, 2)),
                                name_span: dummy_span,
                                name: "None".to_string(),
                                body: StructBody::Unit,
                            }),
                        ],
                    }),
                    Type::Union(Union {
                        version: MajorVersion(2),
                        name_span: dummy_span,
                        name: "Union".to_string(),
                        items: vec![
                            UnionItem::Include(Include {
                                version: MajorVersion(1),
                                items: Vec::new(),
                            }),
                            UnionItem::Field(UnionField {
                                version: None,
                                name_span: dummy_span,
                                name: "None2".to_string(),
                                body: StructBody::Unit,
                            }),
                        ],
                    }),
                    Type::Object(Object::Struct(Struct {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "Object".to_string(),
                        body: StructBody::Items(vec![StructItem::Field(StructField {
                            version: None,
                            name_span: dummy_span,
                            name: "struct_".to_string(),
                            field_type: FieldType::Type(
                                dummy_span,
                                "Struct".to_string(),
                                None,
                                Some(MajorVersion(1))
                            ),
                        }),]),
                    })),
                    Type::Object(Object::Struct(Struct {
                        version: MajorVersion(2),
                        name_span: dummy_span,
                        name: "Object".to_string(),
                        body: StructBody::Tuple(Tuple(vec![TupleItem::Field(TupleField {
                            version: None,
                            field_type: FieldType::Type(
                                dummy_span,
                                "Enum".to_string(),
                                None,
                                Some(MajorVersion(1))
                            ),
                        }),])),
                    })),
                    Type::Object(Object::Union(Union {
                        version: MajorVersion(3),
                        name_span: dummy_span,
                        name: "Object".to_string(),
                        items: vec![UnionItem::Field(UnionField {
                            version: None,
                            name_span: dummy_span,
                            name: "Struct".to_string(),
                            body: StructBody::Items(vec![StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "extern_object".to_string(),
                                field_type: FieldType::Reference(
                                    dummy_span,
                                    Box::new(FieldType::Type(
                                        dummy_span,
                                        "types".to_string(),
                                        Some("Object".to_string()),
                                        None
                                    ))
                                ),
                            }),]),
                        }),],
                    })),
                    Type::Object(Object::Enum(Enum {
                        version: MajorVersion(4),
                        name_span: dummy_span,
                        name: "Object".to_string(),
                        items: vec![EnumItem::Field(EnumField {
                            version: None,
                            name_span: dummy_span,
                            name: "Zero".to_string(),
                            value: None,
                        }),],
                    })),
                    Type::Function(Function {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "Function".to_string(),
                        items: vec![
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "one".to_string(),
                                field_type: FieldType::Type(
                                    dummy_span,
                                    "Struct".to_string(),
                                    None,
                                    Some(MajorVersion(1))
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "two".to_string(),
                                field_type: FieldType::Reference(
                                    dummy_span,
                                    Box::new(FieldType::Type(
                                        dummy_span,
                                        "Object".to_string(),
                                        None,
                                        None,
                                    ))
                                ),
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "three".to_string(),
                                field_type: FieldType::Primitive(dummy_span, Primitive::Bytes),
                            }),
                        ],
                        return_type: Some(FieldType::Type(
                            dummy_span,
                            "Struct".to_string(),
                            None,
                            Some(MajorVersion(1))
                        )),
                    }),
                    Type::Command(Command {
                        version: MajorVersion(1),
                        name_span: dummy_span,
                        name: "Command".to_string(),
                        items: vec![
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "one".to_string(),
                                field_type: FieldType::Reference(
                                    dummy_span,
                                    Box::new(FieldType::Type(
                                        dummy_span,
                                        "Object".to_string(),
                                        None,
                                        None,
                                    ))
                                )
                            }),
                            StructItem::Field(StructField {
                                version: None,
                                name_span: dummy_span,
                                name: "two".to_string(),
                                field_type: FieldType::Reference(
                                    dummy_span,
                                    Box::new(FieldType::Type(
                                        dummy_span,
                                        "types".to_string(),
                                        Some("Object".to_string()),
                                        None,
                                    ))
                                )
                            }),
                        ],
                    }),
                ],
            },
        );
    }
}
