package excel.engine.model.message;

import java.io.Serializable;

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
}
