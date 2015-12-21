package me.excel.tools.model.excel;

import java.util.List;

/**
 * excel row
 *
 * Created by hanwen on 15-12-16.
 */
public interface ExcelRow {

  /**
   * row 所在行数
   * @return 1-based
   */
  int getRowNum();

  /**
   * row 所在sheet
   * @return
   */
  ExcelSheet getSheet();

  /**
   * row 中所有的cell
   * @return
   */
  List<ExcelCell> getCells();

  /**
   * row 中所有的cell数量
   * @return
   */
  int sizeOfCells();

  /**
   * 根据 index 得到cell
   * @param index
   * @return
   */
  ExcelCell getCell(int index);

  /**
   * 增加一个 cell
   * @param excelCell
   * @return
   */
  boolean addCell(ExcelCell excelCell);

  /**
   * row 中最后一个cell
   * @return
   */
  ExcelCell getLastCell();

  /**
   * 根据 field 得到cell
   * @param field
   * @return
   */
  ExcelCell getCell(String field);

}
