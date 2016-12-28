package me.excel.tools.processor;

import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;

import java.util.List;

/**
 * listener of workbook to object processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ObjectProcessorListener {

  /**
   * before model created
   *
   * @param sheet   sheet
   * @param objects empty object list
   */
  void beforeSheet(Sheet sheet, List<Object> objects);

  /**
   * before object value set
   *
   * @param row    row
   * @param object value not set
   */
  void beforeRow(Row row, Object object);

  /**
   * after object value set
   *
   * @param row    row
   * @param object value set
   */
  void afterRow(Row row, Object object);

  /**
   * after all object value set
   *
   * @param sheet   sheet
   * @param objects value set object list
   */
  void afterSheet(Sheet sheet, List<Object> objects);

  int getSheetIndex();
}
