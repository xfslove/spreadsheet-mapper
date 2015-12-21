package me.excel.tools.model.excel;

import java.util.List;

/**
 * excel sheet
 *
 * Created by hanwen on 15-12-16.
 */
public interface ExcelSheet {

  /**
   * 得到sheet的名称
   * @return
   */
  String getSheetName();

  /**
   * sheet 所有的row（包括fields和数据）
   * @return
   */
  List<ExcelRow> getRows();

  /**
   * sheet 所有row的数量
   * @return
   */
  int sizeOfRows();

  /**
   * 根据 index 获得row
   * @param index
   * @return
   */
  ExcelRow getRow(int index);

  /**
   * sheet 所有的数据row
   * @return
   */
  List<ExcelRow> getDataRows();

  /**
   * 增加一个row
   * @param excelRow
   * @return
   */
  boolean addRow(ExcelRow excelRow);

  /**
   * 得到最后一个row
   * @return
   */
  ExcelRow getLastRow();

  /**
   * sheet 所在workbook
   * @return
   */
  ExcelWorkbook getWorkbook();

  /**
   * sheet 所在位置
   * @return 1-based
   */
  int getIndex();

  /**
   * 是否有comment
   * @return
   */
  boolean hasComments();
}
