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
   * @return row num 1-based
   */
  int getRowNum();

  /**
   * @return sheet of row at
   */
  ExcelSheet getSheet();

  /**
   * @return cells of this row
   */
  List<ExcelCell> getCells();

  /**
   * @return cells size of this row
   */
  int sizeOfCells();

  /**
   * get cell by index
   *
   * @param index 1-based
   * @return cell
   */
  ExcelCell getCell(int index);

  /**
   * add cell
   *
   * @param excelCell cell
   * @return success
   */
  boolean addCell(ExcelCell excelCell);

  /**
   * @return first cell of this row
   */
  ExcelCell getFirstCell();

  /**
   * @return last cell of this row
   */
  ExcelCell getLastCell();

  /**
   * get cell by field (see {@link ExcelCell#getField()})
   *
   * @param field field
   * @return cell
   */
  ExcelCell getCell(String field);

}
