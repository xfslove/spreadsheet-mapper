package me.excel.tools.model.excel;

/**
 * excel cell
 *
 * Created by hanwen on 15-12-16.
 */
public interface ExcelCell {

  /**
   * cell 所在行数
   * @return 1-based
   */
  int getRowNum();

  /**
   * cell 所在列数
   * @return 1-based
   */
  int getColumnNum();

  /**
   * cell 对应的字段
   * @return
   */
  String getField();

  /**
   * 得到cell的值
   * @return
   */
  String getValue();

  /**
   * cell 所在行
   * @return
   */
  ExcelRow getRow();

  /**
   * cell 的备注
   * @return
   */
  ExcelCellComment getComment();

  /**
   * cell 所在sheet
   * @return
   */
  ExcelSheet getSheet();

  void setComment(ExcelCellComment comment);

  /**
   * 将一些value转换成readable的
   * @param value
   */
  void convertToReadableValue(String value);
}
