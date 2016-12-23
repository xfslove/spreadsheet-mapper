package me.excel.tools.model.excel;

import java.io.Serializable;
import java.util.List;

/**
 * excel row
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelRow extends Serializable {

  /**
   * row num
   *
   * @return 1-based
   */
  int getRowNum();

  /**
   * sheet of row at
   *
   * @return
   */
  ExcelSheet getSheet();

  /**
   * get cells of this row
   *
   * @return
   */
  List<ExcelCell> getCells();

  /**
   * get cells size of this row
   *
   * @return
   */
  int sizeOfCells();

  /**
   * get cell by index
   *
   * @param index 1-based
   * @return
   */
  ExcelCell getCell(int index);

  /**
   * add cell
   *
   * @param excelCell
   * @return
   */
  boolean addCell(ExcelCell excelCell);

  /**
   * get first cell of this row
   *
   * @return
   */
  ExcelCell getFirstCell();

  /**
   * get last cell of this row
   *
   * @return
   */
  ExcelCell getLastCell();

  /**
   * get cell by field (see {@link ExcelCell#getField()})
   *
   * @param field
   * @return
   */
  ExcelCell getCell(String field);

}
