-- Types
inductive Ty : Type where
  | base : Ty
  | arrow : Ty → Ty → Ty

-- Terms
inductive Term : Type where
  | var : Nat → Term
  | app : Term → Term → Term
  | lam : Ty → Term → Term
  | base : Term


-- Contexts
abbrev Context := List Ty

-- Typing relation
inductive HasType : Context → Term → Ty → Prop where
  | tvar : ∀ (Γ : Context) (x : Nat) (T : Ty),
      Γ[x]? = some T →
      HasType Γ (Term.var x) T
  | tapp:
      HasType Γ t₁ (Ty.arrow τ₁ τ₂) →
      HasType Γ t₂ τ₁ →
      HasType Γ (Term.app t₁ t₂) τ₂
  | tlam:
      HasType (τ₁::Γ) t₁ τ₂ →
      HasType Γ (Term.lam τ₁ t₁) (Ty.arrow τ₁ τ₂)
  | tbase:
      HasType Γ (Term.base) Ty.base

def subst: Term → Nat → Term → Term := sorry

inductive E: Ty → Term → Prop

inductive Value: Ty → Term → Prop where
  | vbase : Value Ty.base Term.base
  | vlam : ∀ {τ₁ τ₂ e} ,
    (∀ v, Value τ₁ v → E τ₂ (subst e 0 v)) →
    Value (Ty.arrow τ₁ τ₂) (Term.lam τ₁ e)
