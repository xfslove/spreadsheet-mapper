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
   * cell comment
   *
   * @return
   */
  String getComment();

  /**
   * set comment
   *
   * @param comment
   */
  void setComment(String comment);

  /**
   * comment's rectangle length
   *
   * @return
   */
  int getLength();

  /**
   * comment's rectangle height
   *
   * @return
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
   * @param sheetIndex
   */
  void setSheetIndex(int sheetIndex);

  /**
   * set row num
   *
   * @param rowNum
   */
  void setRowNum(int rowNum);

  /**
   * set column num
   *
   * @param columnNum
   */
  void setColumnNum(int columnNum);

  /**
   * set belong cell
   *
   * @param cell
   */
  void setCell(ExcelCell cell);
}
