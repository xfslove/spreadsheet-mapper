package java.excel.engine.model.message;

import java.io.Serializable;
import java.util.Set;

/**
 * valid result message
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public interface ValidateMessage extends Serializable {

  /**
   * @return valid message
   */
  String getMessage();

  Set<String> getMessageOnFields();
}
