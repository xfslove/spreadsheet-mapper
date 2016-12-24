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
   * @return field
   */
  String getField();

  /**
   * cell value
   *
   * @return value
   */
  String getValue();

  /**
   * @return sheet of cell at
   */
  ExcelSheet getSheet();

  /**
   * @return row of cell at
   */
  ExcelRow getRow();

  /**
   * @return cell comment
   */
  ExcelCellComment getComment();

  /**
   * set comment at the cell
   *
   * @param comment comment
   */
  void setComment(ExcelCellComment comment);
}
