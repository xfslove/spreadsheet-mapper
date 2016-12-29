package java.excel.engine.model.excel;

import java.util.List;

/**
 * excel row
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface Row extends ExcelMeta {

  /**
   * @return row num 1-based
   */
  int getIndex();

  /**
   * @return sheet of row at
   */
  Sheet getSheet();

  /**
   * @return cells of this row
   */
  List<Cell> getCells();

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
  Cell getCell(int index);

  /**
   * get cell by field
   *
   * @param field field
   * @return cell
   */
  Cell getCell(String field);

  /**
   * add cell
   *
   * @param cell cell
   * @return success
   */
  boolean addCell(Cell cell);

  /**
   * @return first cell of this row
   */
  Cell getFirstCell();

  /**
   * @return last cell of this row
   */
  Cell getLastCell();

}
