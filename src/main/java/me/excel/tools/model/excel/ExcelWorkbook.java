package me.excel.tools.model.excel;

import java.io.Serializable;
import java.util.List;

/**
 * excel workbook
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelWorkbook extends Serializable {

  /**
   * is xlsx or xls
   *
   * @return true if xlsx
   */
  boolean isAfter97();

  /**
   * set xlsx or xls
   *
   * @param after97 true if xlsx
   */
  void setAfter97(boolean after97);

  /**
   * @return sheets of this workbook
   */
  List<ExcelSheet> getSheets();

  /**
   * @return sheets size of this workbook
   */
  int sizeOfSheets();

  /**
   * add sheet
   *
   * @param excelSheet sheet
   * @return success
   */
  boolean addSheet(ExcelSheet excelSheet);

  /**
   * get sheet by index
   *
   * @param index 1-based
   * @return sheet
   */
  ExcelSheet getSheet(int index);

  /**
   * @return last sheet
   */
  ExcelSheet getLastSheet();

  /**
   * @return first Sheet
   */
  ExcelSheet getFirstSheet();
}
