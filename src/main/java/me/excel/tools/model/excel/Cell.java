package me.excel.tools.model.excel;

/**
 * excel cell
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface Cell extends ExcelMeta {

  /**
   *
   * @return the cell corresponding field
   */
  String getField();

  /**
   *
   * @param field
   */
  void setField(String field);

  /**
   * cell value
   *
   * @return value
   */
  String getValue();

  /**
   * cell row index
   *
   * @return 1-based
   */
  int getRowIndex();

  /**
   * cell column index
   *
   * @return 1-based
   */
  int getColumnIndex();

  /**
   * @return sheet of cell at
   */
  Sheet getSheet();

  /**
   * @return row of cell at
   */
  Row getRow();
}
