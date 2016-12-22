package me.excel.tools.model.excel;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

/**
 * excel sheet
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelSheet extends Serializable {

  /**
   * sheet name
   *
   * @return
   */
  String getSheetName();

  /**
   * get rows of this sheet
   *
   * @return
   */
  List<ExcelRow> getRows();

  /**
   * get rows zise of this sheet
   *
   * @return
   */
  int sizeOfRows();

  /**
   * get row by index
   *
   * @param index 1-based
   * @return
   */
  ExcelRow getRow(int index);

  /**
   * get data rows of this sheet (exclude first, second, third rows)
   *
   * @return
   */
  List<ExcelRow> getDataRows();

  /**
   * add row
   *
   * @param excelRow
   * @return
   */
  boolean addRow(ExcelRow excelRow);

  /**
   * get last row
   *
   * @return
   */
  ExcelRow getLastRow();

  /**
   * workbook of sheet at
   *
   * @return
   */
  ExcelWorkbook getWorkbook();

  /**
   * get sheet index
   *
   * @return 1-based
   */
  int getIndex();

  /**
   * has comment
   *
   * @return
   */
  boolean hasComments();

  /**
   * get all fields
   *
   * @return
   */
  List<String> getKeyRowFields();

  /**
   * collect all values of supplied field (distinct)
   *
   * @param field
   * @return
   */
  Set<String> getDistinctCellValuesOfField(String field);
}
