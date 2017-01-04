package spread.sheet.model.core;

import java.io.Serializable;
import java.util.List;

/**
 * sheet
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface Sheet extends Serializable, Comparable<Sheet> {

  /**
   * get sheet index
   *
   * @return 1-based
   */
  int getIndex();

  /**
   * @return name of sheet
   */
  String getName();

  /**
   * @return rows of this sheet
   */
  List<Row> getRows();

  /**
   * @return rows size of this sheet
   */
  int sizeOfRows();

  /**
   * get row by index
   *
   * @param index 1-based
   * @return row
   */
  Row getRow(int index);

  /**
   * add row
   *
   * @param row row
   * @return true if success
   */
  boolean addRow(Row row);

  /**
   * @return first row
   */
  Row getFirstRow();

  /**
   * @return last row
   */
  Row getLastRow();

  /**
   * @return the workbook of this
   */
  Workbook getWorkbook();
}
