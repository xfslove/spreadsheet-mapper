package spread.sheet.message;

import spread.sheet.model.message.ErrorMessage;
import spread.sheet.model.message.MessageWriteStrategies;

import java.io.OutputStream;
import java.util.Collection;

/**
 * error message writer
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public interface ErrorMessageWriter {

  /**
   * <pre>
   * {@link MessageWriteStrategy} unique with {@link MessageWriteStrategies} (one to one),
   * if you add message write strategy with same {@link MessageWriteStrategies},
   * after add will override before add
   * </pre>
   *
   * @param messageWriteStrategies message write strategy
   * @see MessageWriteStrategy
   */
  ErrorMessageWriter messageWriteStrategy(MessageWriteStrategy... messageWriteStrategies);

  /**
   * write supplied message to file
   *
   * @param errorMessages collection of error message
   * @param outputStream  intend to write out stream
   */
  void write(Collection<ErrorMessage> errorMessages, OutputStream outputStream);
}
