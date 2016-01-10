package me.excel.tools.model.excel;

import java.util.List;

/**
 * excel comment
 *
 * Created by hanwen on 15-12-17.
 */
public interface ExcelCellComment {

  /**
   * comment 内容
   * @return
   */
  List<String> getComments();

  /**
   * 增加comment
   *
   * @param comment
   */
  void addComment(String comment);

  /**
   * comment 的长度
   * @return
   */
  int getLength();

  /**
   * comment 的宽度
   * @return
   */
  int getHeight();

  /**
   * comment 对应的cell
   * @return
   */
  ExcelCell getCell();

  /**
   * comment 所在sheet
   * @return
   */
  ExcelSheet getSheet();
}
