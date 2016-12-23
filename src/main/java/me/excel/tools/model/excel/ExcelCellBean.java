package me.excel.tools.model.excel;

import me.excel.tools.ExcelSupportedDateFormat;
import me.excel.tools.model.comment.ExcelCellComment;
import me.excel.tools.model.comment.ExcelCellCommentBean;
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

  private String field;

  private String value;

  private ExcelCellComment excelCellComment;

  public ExcelCellBean(int rowIndex, int columnIndex, String field, String value) {
    this.rowIndex = rowIndex;
    this.columnIndex = columnIndex;
    this.field = field;
    this.value = value;
  }

  public ExcelCellBean(Cell cell, String field) {

    this.rowIndex = cell.getRowIndex() + 1;
    this.columnIndex = cell.getColumnIndex() + 1;
    this.field = field;

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

  public static ExcelCellBean emptyCell(int rowIndex, int columnIndex, String field) {
    return new ExcelCellBean(rowIndex, columnIndex, field);
  }

  private ExcelCellBean(int rowIndex, int columnIndex, String field) {
    this.rowIndex = rowIndex;
    this.columnIndex = columnIndex;
    this.field = field;
    this.value = null;
  }

  @Override
  public int getRowNum() {
    return rowIndex;
  }

  @Override
  public int getColumnNum() {
    return columnIndex;
  }

  @Override
  public String getField() {
    return field;
  }

  @Override
  public String getValue() {
    return value;
  }

  public void setRow(ExcelRow excelRow) {
    this.excelRow = excelRow;
  }

  @Override
  public ExcelRow getRow() {
    return excelRow;
  }

  @Override
  public ExcelCellComment getComment() {
    return excelCellComment;
  }

  @Override
  public ExcelSheet getSheet() {
    return excelRow.getSheet();
  }

  @Override
  public void setComment(ExcelCellComment excelCellComment) {
    excelCellComment.setCell(this);
    this.excelCellComment = excelCellComment;
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
        Objects.equals(field, cell.field) &&
        Objects.equals(value, cell.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(rowIndex, columnIndex, field, value);
  }
}
