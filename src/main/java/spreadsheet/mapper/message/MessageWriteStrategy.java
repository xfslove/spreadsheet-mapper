package spreadsheet.mapper.message;

import org.apache.poi.ss.usermodel.Workbook;
import spreadsheet.mapper.model.message.ErrorMessage;

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
   * @param workbook      error message write workbook
   * @param errorMessages collection of error message with same message write strategy
   */
  void write(Workbook workbook, Collection<ErrorMessage> errorMessages);
}
