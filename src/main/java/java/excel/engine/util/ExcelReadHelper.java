package java.excel.engine.util;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.excel.engine.exception.ExcelReadException;
import java.excel.engine.exception.ExcelTemplateException;
import java.excel.engine.model.excel.*;
import java.excel.engine.model.ext.SheetTemplate;
import java.excel.engine.model.ext.SheetTemplateBean;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExcelReadHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelReadHelper.class);

  private ExcelReadHelper() {
    // default constructor
  }

  /**
   * read supplied excel stream to {@link Workbook} according to template
   *
   * @param inputStream         auto close
   * @param sheetIndex2template sheet index 2 sheet template
   * @return workbook
   * @throws IOException io exception
   */
  public static Workbook read(InputStream inputStream, Map<Integer, SheetTemplate> sheetIndex2template) throws IOException {

    Workbook excelWorkbook = new WorkbookBean();

    try {

      if (inputStream.available() == 0) {
        return excelWorkbook;
      }

      org.apache.poi.ss.usermodel.Workbook workbook = WorkbookFactory.create(inputStream);

      if (workbook instanceof HSSFWorkbook) {
        excelWorkbook.setAfter97(false);
      }

      int sheetCount = workbook.getNumberOfSheets();

      for (int i = 0; i < sheetCount; i++) {

        org.apache.poi.ss.usermodel.Sheet sheet = workbook.getSheetAt(i);

        if (sheet == null) {
          continue;
        }

        Sheet excelSheet = createSheet(sheet);

        SheetTemplate template = sheetIndex2template.get(excelSheet.getIndex());
        if (template != null) {

          if (template.getFieldHeaderMeta() == null) {
            LOGGER.error("sheet template error, not has field row");
            throw new ExcelTemplateException("not has field row");
          }

        } else {

          template = SheetTemplateBean.DEFAULT(excelSheet.getIndex());
        }

        excelSheet.setTemplate(template);
        excelWorkbook.addSheet(excelSheet);

        org.apache.poi.ss.usermodel.Row fieldRow = sheet.getRow(excelSheet.getTemplate().getFieldHeaderMeta().getRowIndex() - 1);
        Map<Integer, String> columnIndex2field = buildColumnIndex2fieldMap(fieldRow);

        int lastRowNum = sheet.getLastRowNum();
        for (int j = 0; j < lastRowNum; j++) {

          org.apache.poi.ss.usermodel.Row row = sheet.getRow(j);

          Row excelRow = createRow(row);
          excelSheet.addRow(excelRow);

          List<Cell> cells = createRowCells(row);

          for (Cell cell : cells) {

            cell.setField(columnIndex2field.get(cell.getColumnIndex()));
            excelRow.addCell(cell);
          }

        }
      }

      return excelWorkbook;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelReadException(e);
    } finally {
      inputStream.close();
    }
  }

  private static Sheet createSheet(org.apache.poi.ss.usermodel.Sheet sheet) {
    return new SheetBean(sheet);
  }

  private static Row createRow(org.apache.poi.ss.usermodel.Row row) {
    return new RowBean(row);
  }

  private static List<Cell> createRowCells(org.apache.poi.ss.usermodel.Row row) {
    List<Cell> cells = new ArrayList<>();

    int maxColNum = row.getLastCellNum() > 0 ? row.getLastCellNum() : 0;

    for (int k = 0; k < maxColNum; k++) {

      org.apache.poi.ss.usermodel.Cell cell = row.getCell(k);

      CellBean excelCell;
      if (cell == null) {

        excelCell = CellBean.EMPTY_CELL(k + 1, row.getRowNum() + 1);
      } else {

        excelCell = new CellBean(cell);
      }

      cells.add(excelCell);
    }

    return cells;
  }

  private static Map<Integer, String> buildColumnIndex2fieldMap(org.apache.poi.ss.usermodel.Row row) {
    Map<Integer, String> columnIndex2field = new HashMap<>();

    List<Cell> cells = createRowCells(row);

    for (Cell cell : cells) {
      columnIndex2field.put(cell.getColumnIndex(), cell.getValue());
    }

    return columnIndex2field;
  }

}
