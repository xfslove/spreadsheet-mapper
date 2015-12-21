package me.excel.tools.model.excel;

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
  String getComment();

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
