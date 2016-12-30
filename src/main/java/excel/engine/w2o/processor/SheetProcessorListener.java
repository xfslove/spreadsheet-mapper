package excel.engine.w2o.processor;

import excel.engine.model.core.Row;
import excel.engine.model.core.Sheet;

import java.util.List;

/**
 * listener of workbook to object processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetProcessorListener {

  /**
   * before model created
   *
   * @param sheet sheet
   */
  void beforeSheet(Sheet sheet);

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
}
