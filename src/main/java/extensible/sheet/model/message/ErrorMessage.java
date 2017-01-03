package extensible.sheet.model.message;

import java.io.Serializable;

/**
 * validate error message
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public interface ErrorMessage extends Serializable {

  /**
   * if use {@link MessageWriteStrategies#TEXT_BOX}, {@link #getRowIndex()} &amp; {@link #getColumnIndex()} will ignore
   *
   * @return use which message write strategy to write error message
   * @see MessageWriteStrategies
   */
  String getMessageWriteStrategy();

  /**
   * @return message show on which sheet
   */
  int getSheetIndex();

  /**
   * @return message on which row
   */
  Integer getRowIndex();

  /**
   * @return message on which column
   */
  Integer getColumnIndex();

  /**
   * @return valid error message
   */
  String getErrorMessage();
}
