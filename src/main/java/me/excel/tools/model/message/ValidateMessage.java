package me.excel.tools.model.message;

import java.io.Serializable;

/**
 * validate result message
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public interface ValidateMessage extends Serializable {

  /**
   * when collect error messages will skip those success result
   *
   * @return validate result
   */
  ValidateResult getResult();

  /**
   * @return validate message
   */
  String getMessage();
}
