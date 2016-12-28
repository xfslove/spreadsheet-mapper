package me.excel.tools.model.excel;

import me.excel.tools.model.template.SheetHeader;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

/**
 * excel sheet
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface Sheet extends Serializable {

  /**
   * excel sheet header
   *
   * @return header
   * @see SheetHeader
   */
  SheetHeader getHeader();

  /**
   * set header
   *
   * @param header header
   */
  void setHeader(SheetHeader header);

  /**
   * get sheet index
   *
   * @return 1-based
   */
  int getIndex();

  /**
   * @return sheet name
   */
  String getName();

  /**
   * @return rows of this sheet
   */
  List<Row> getRows();

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
  Row getRow(int index);

  /**
   * add row
   *
   * @param row row
   * @return success
   */
  boolean addRow(Row row);

  /**
   * @return first row
   */
  Row getFirstRow();

  /**
   * @return last row
   */
  Row getLastRow();

  /**
   * @return workbook
   */
  Workbook getWorkbook();

  /**
   * @return field row
   */
  Row getFieldRow();

  /**
   * @return data rows
   */
  List<Row> getDataRows();

  /**
   * @param field field
   * @return distinct values of supplied field
   */
  Set<String> getDistinctValuesOfField(String field);

}
