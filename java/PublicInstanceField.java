package experimentation.java;

public class PublicInstanceField {
    public PublicInstanceField (Long x, Long y) {
	this.x = x + y;
    }
    public Long x = 5L;
    public Long plus() { return x + 1; }
    public Number n(boolean b) {
	if(b) {return 3; } else {return 3.0;}
    }
    public void m() {}
    public static void m(int i) {}
    public static java.util.function.Function<Long, Long> longId = (x) -> x;
    public java.util.function.Function<Long, Long> instanceFuncField = (arg) -> x + arg;
}
