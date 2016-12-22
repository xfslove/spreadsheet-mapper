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
   * get sheets of this workbook
   *
   * @return
   */
  List<ExcelSheet> getSheets();

  /**
   * get sheets size of this workbook
   *
   * @return
   */
  int sizeOfSheets();

  /**
   * add sheet
   *
   * @param excelSheet
   * @return
   */
  boolean addSheet(ExcelSheet excelSheet);

  /**
   * get sheet by index
   *
   * @param index 1-based
   * @return
   */
  ExcelSheet getSheet(int index);

  /**
   * get last sheet
   *
   * @return
   */
  ExcelSheet getLastSheet();

  /**
   * get first Sheet
   *
   * @return
   */
  ExcelSheet getFirstSheet();
}
