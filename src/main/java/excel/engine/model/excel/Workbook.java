package excel.engine.model.excel;

import java.util.List;

/**
 * excel workbook
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface Workbook extends ExcelMeta {

  /**
   * is xlsx or xls, default is xlsx
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
  List<Sheet> getSheets();

  /**
   * @return sheets size of this workbook
   */
  int sizeOfSheets();

  /**
   * add sheet
   *
   * @param sheet sheet
   * @return true if success
   */
  boolean addSheet(Sheet sheet);

  /**
   * get sheet by index
   *
   * @param index 1-based
   * @return sheet
   */
  Sheet getSheet(int index);

  /**
   * @return last sheet
   */
  Sheet getLastSheet();

  /**
   * @return first Sheet
   */
  Sheet getFirstSheet();
}
