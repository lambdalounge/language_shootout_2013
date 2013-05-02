package kata;

import java.nio.CharBuffer;

public interface Screener {

    void process(CharBuffer reader);

    Statistics stats();

}
