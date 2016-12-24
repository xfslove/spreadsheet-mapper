package me.excel.tools.model.comment;

import me.excel.tools.model.excel.ExcelCell;

import java.io.Serializable;

/**
 * excel cell comment
 * <p>
 * Created by hanwen on 15-12-17.
 */
public interface ExcelCellComment extends Serializable {

  /**
   * @return comment
   */
  String getComment();

  /**
   * set comment
   *
   * @param comment comment
   */
  void setComment(String comment);

  /**
   * @return comment's rectangle length
   */
  int getLength();

  /**
   * @return comment's rectangle height
   */
  int getHeight();

  /**
   * sheet index of comment location
   *
   * @return 1-based
   */
  int getSheetIndex();

  /**
   * row num of comment location
   *
   * @return 1-based
   */
  int getRowNum();

  /**
   * column num of comment location
   *
   * @return 1-based
   */
  int getColumnNum();

  /**
   * set sheet index
   *
   * @param sheetIndex sheet index
   */
  void setSheetIndex(int sheetIndex);

  /**
   * set row num
   *
   * @param rowNum row num
   */
  void setRowNum(int rowNum);

  /**
   * set column num
   *
   * @param columnNum column num
   */
  void setColumnNum(int columnNum);

  /**
   * set belong cell
   *
   * @param cell cell
   */
  void setCell(ExcelCell cell);
}
