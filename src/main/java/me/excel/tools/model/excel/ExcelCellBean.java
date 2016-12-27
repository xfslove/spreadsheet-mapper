package me.excel.tools.model.excel;

import me.excel.tools.ExcelSupportedDateFormat;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.util.NumberToTextConverter;

import java.text.SimpleDateFormat;
import java.util.Objects;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelCellBean implements ExcelCell {

  private ExcelRow excelRow;

  private int rowIndex;

  private int columnIndex;

  private String value;

  private String field;

  public ExcelCellBean(int rowIndex, int columnIndex, String value) {
    this.rowIndex = rowIndex;
    this.columnIndex = columnIndex;
    this.value = value;
  }

  @Override
  public String getField() {
    return field;
  }

  @Override
  public void setField(String field) {
    this.field = field;
  }

  public ExcelCellBean(Cell cell) {

    this.rowIndex = cell.getRowIndex() + 1;
    this.columnIndex = cell.getColumnIndex() + 1;

    if (cell.getCellType() == Cell.CELL_TYPE_BLANK) {

      value = null;

    } else if (cell.getCellType() == Cell.CELL_TYPE_BOOLEAN) {

      value = Boolean.toString(cell.getBooleanCellValue());

    } else if (cell.getCellType() == Cell.CELL_TYPE_ERROR) {

      value = null;

    } else if (cell.getCellType() == Cell.CELL_TYPE_FORMULA) {

      value = null;

    } else if (cell.getCellType() == Cell.CELL_TYPE_NUMERIC) {

      if (DateUtil.isCellDateFormatted(cell)) {
        String dateFormat = ExcelSupportedDateFormat.getDateFormat(cell.getCellStyle().getDataFormatString());

        if (dateFormat == null) {
          value = ExcelSupportedDateFormat.ERROR_PATTERN;
        } else {
          SimpleDateFormat simpleDateFormat = new SimpleDateFormat(dateFormat);
          value = simpleDateFormat.format(cell.getDateCellValue());
        }
      } else {
        value = NumberToTextConverter.toText(cell.getNumericCellValue());
      }

    } else if (cell.getCellType() == Cell.CELL_TYPE_STRING) {

      String cellValue = cell.getStringCellValue();
      value = StringUtils.isBlank(cellValue) ? null : cellValue.trim();

    } else {

      value = null;

    }

  }

  public static ExcelCellBean EMPTY_CELL(int rowIndex, int columnIndex) {
    return new ExcelCellBean(rowIndex, columnIndex, null);
  }

  @Override
  public int getRowIndex() {
    return rowIndex;
  }

  @Override
  public int getColumnIndex() {
    return columnIndex;
  }

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public ExcelRow getRow() {
    return excelRow;
  }

  @Override
  public ExcelSheet getSheet() {
    return excelRow.getSheet();
  }

  void setRow(ExcelRow excelRow) {
    this.excelRow = excelRow;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }
    if (obj == null || obj.getClass() != getClass()) {
      return false;
    }
    ExcelCellBean cell = (ExcelCellBean) obj;
    return Objects.equals(rowIndex, cell.rowIndex) &&
        Objects.equals(columnIndex, cell.columnIndex) &&
        Objects.equals(value, cell.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(rowIndex, columnIndex, value);
  }
}
