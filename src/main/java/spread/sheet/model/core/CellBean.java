package spread.sheet.model.core;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.util.NumberToTextConverter;
import spread.sheet.utils.DateFormatRegister;

import java.text.SimpleDateFormat;
import java.util.Objects;

/**
 * Created by hanwen on 15-12-16.
 */
public class CellBean implements Cell {

  private Row row;

  private int columnIndex;

  private String value;

  public CellBean(int columnIndex, String value) {
    this.columnIndex = columnIndex;
    this.value = value;
  }

  public CellBean(org.apache.poi.ss.usermodel.Cell cell) {

    this.columnIndex = cell.getColumnIndex() + 1;

    if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_BLANK) {

      value = null;

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_BOOLEAN) {

      value = Boolean.toString(cell.getBooleanCellValue());

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_ERROR) {

      value = null;

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_FORMULA) {

      value = null;

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_NUMERIC) {

      if (DateUtil.isCellDateFormatted(cell)) {
        String dateFormat = DateFormatRegister.get(cell.getCellStyle().getDataFormatString());

        if (dateFormat == null) {
          value = DateFormatRegister.ERROR_PATTERN;
        } else {
          SimpleDateFormat simpleDateFormat = new SimpleDateFormat(dateFormat);
          value = simpleDateFormat.format(cell.getDateCellValue());
        }
      } else {
        value = NumberToTextConverter.toText(cell.getNumericCellValue());
      }

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_STRING) {

      String cellValue = cell.getStringCellValue();
      value = StringUtils.isBlank(cellValue) ? null : cellValue.trim();

    } else {

      value = null;

    }

  }

  public static CellBean EMPTY_CELL(int columnIndex) {
    return new CellBean(columnIndex, null);
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
  public Row getRow() {
    return row;
  }

  void setRow(Row row) {
    this.row = row;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }
    if (obj == null || obj.getClass() != getClass()) {
      return false;
    }
    CellBean cell = (CellBean) obj;
    return Objects.equals(columnIndex, cell.columnIndex) &&
        Objects.equals(value, cell.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(columnIndex, value);
  }

  @Override
  public int compareTo(Cell o) {
    return new CompareToBuilder()
        .append(columnIndex, o.getColumnIndex()).toComparison();
  }
}
