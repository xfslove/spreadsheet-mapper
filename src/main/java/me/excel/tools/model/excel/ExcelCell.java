package me.excel.tools.model.excel;

import me.excel.tools.model.comment.ExcelCellComment;

import java.io.Serializable;

/**
 * excel cell
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelCell extends Serializable {

  /**
   * cell row num
   *
   * @return 1-based
   */
  int getRowNum();

  /**
   * cell column num
   *
   * @return 1-based
   */
  int getColumnNum();

  /**
   * cell belong field
   *
   * @return
   */
  String getField();

  /**
   * cell value
   *
   * @return
   */
  String getValue();

  /**
   * sheet of cell at
   *
   * @return
   */
  ExcelSheet getSheet();

  /**
   * row of cell at
   *
   * @return
   */
  ExcelRow getRow();

  /**
   * cell comment
   *
   * @return
   */
  ExcelCellComment getComment();

  /**
   * set comment at the cell
   *
   * @param comment
   */
  void setComment(ExcelCellComment comment);
}
