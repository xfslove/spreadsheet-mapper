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
   * @return sheet name
   */
  String getSheetName();

  /**
   * @return rows of this sheet
   */
  List<ExcelRow> getRows();

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
  ExcelRow getRow(int index);

  /**
   * @return data rows of this sheet (exclude first, second, third rows)
   */
  List<ExcelRow> getDataRows();

  /**
   * @return data size of this sheet
   */
  int sizeOfData();

  /**
   * add row
   *
   * @param excelRow row
   * @return success
   */
  boolean addRow(ExcelRow excelRow);

  /**
   * @return first row
   */
  ExcelRow getFirstRow();

  /**
   * @return last row
   */
  ExcelRow getLastRow();

  /**
   * @return workbook of sheet at
   */
  ExcelWorkbook getWorkbook();

  /**
   * get sheet index
   *
   * @return 1-based
   */
  int getIndex();

  /**
   * @return has comment
   */
  boolean hasComments();

  /**
   * collect all values of supplied field (distinct)
   *
   * @param field field
   * @return values of field
   */
  Set<String> getDistinctCellValuesByField(String field);
}
