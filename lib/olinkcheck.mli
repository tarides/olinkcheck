module Markdown : sig
  include module type of Markdown
end

module Plaintext : sig
  include module type of Plaintext
end

module Link : sig
  include module type of Link
end

module Sexp : sig
  include module type of Sexp
end
