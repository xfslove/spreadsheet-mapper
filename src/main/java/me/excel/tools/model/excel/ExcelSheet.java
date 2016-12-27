package me.excel.tools.model.excel;

import me.excel.tools.model.template.ExcelSheetHeaderInfo;

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
   * excel sheet header info
   *
   * @return header info
   * @see ExcelSheetHeaderInfo
   */
  ExcelSheetHeaderInfo getHeaderInfo();

  /**
   * set header info
   *
   * @param headerInfo header info
   */
  void setHeaderInfo(ExcelSheetHeaderInfo headerInfo);

  /**
   * get sheet index
   *
   * @return 1-based
   */
  int getIndex();

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
   * @return data rows
   */
  List<ExcelRow> getDataRows();

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
   * @param field field
   * @return distinct values of supplied field
   */
  Set<String> getDistinctValuesOfField(String field);

}
