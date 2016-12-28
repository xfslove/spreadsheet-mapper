package me.excel.tools.model.template;

import java.io.Serializable;

/**
 * detail of sheet header {@link SheetHeader}
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface HeaderDetail extends Serializable {

  /**
   * @return the header field
   */
  String getField();

  /**
   * @return the header title
   */
  String getTitle();

  /**
   * @return the header prompt
   */
  String getPrompt();
}
