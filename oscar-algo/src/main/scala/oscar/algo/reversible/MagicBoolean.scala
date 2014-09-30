package oscar.algo.reversible

class MagicBoolean(context: ReversibleContext, default: Boolean) {

  private final var magic: Long = -1
  private final var b: Boolean = default
  
  @inline final def value_=(b: Boolean): Unit = {
    magic = context.magic
    this.b = b
  }
  
  @inline final def value: Boolean = {
    if (magic == context.magic) b
    else {
      magic = context.magic
      b = default
      default
    }
  }
}