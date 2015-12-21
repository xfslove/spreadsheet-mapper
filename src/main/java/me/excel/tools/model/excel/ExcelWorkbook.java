package me.excel.tools.model.excel;

import java.util.List;

/**
 * excel workbook
 *
 * Created by hanwen on 15-12-16.
 */
public interface ExcelWorkbook {

  /**
   * workbook 中所有sheet
   * @return
   */
  List<ExcelSheet> getSheets();

  /**
   * workbook 所有sheet数量
   * @return
   */
  int sizeOfSheets();

  /**
   * 增加一个sheet
   * @param excelSheet
   * @return
   */
  boolean addSheet(ExcelSheet excelSheet);

  /**
   * 根据 index 得到sheet
   * @param index
   * @return
   */
  ExcelSheet getSheet(int index);

  /**
   * 得到最后一个sheet
   * @return
   */
  ExcelSheet getLastSheet();
}
