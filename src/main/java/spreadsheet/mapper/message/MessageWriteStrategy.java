package spreadsheet.mapper.message;

import spreadsheet.mapper.model.message.ErrorMessage;

import java.io.OutputStream;
import java.util.Collection;

/**
 * message write strategy
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public interface MessageWriteStrategy {

  /**
   * @return which strategy use of this
   */
  String getStrategy();

  /**
   * write error messages
   *
   * @param outputStream  intend to write out stream
   * @param errorMessages collection of error message with same message write strategy
   */
  void write(OutputStream outputStream, Collection<ErrorMessage> errorMessages);
}
