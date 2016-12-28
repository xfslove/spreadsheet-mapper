package me.excel.tools.helper;

import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.excel.Cell;
import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;
import me.excel.tools.model.excel.Workbook;
import me.excel.tools.model.template.SheetHeader;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExcelToWorkbookHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelToWorkbookHelper.class);

  private ExcelToWorkbookHelper() {
    // default constructor
  }

  /**
   * read supplied excel stream to {@link Workbook}
   *
   * @param inputStream  auto close
   * @param sheetHeaders sheet header infos, null means using default header
   * @return workbook
   * @throws IOException io exception
   */
  public static Workbook read(InputStream inputStream, SheetHeader... sheetHeaders) throws IOException {

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

      Map<Integer, SheetHeader> sheetIndex2header = buildHeaderMap(sheetHeaders);
      for (int i = 0; i < sheetCount; i++) {

        org.apache.poi.ss.usermodel.Sheet sheet = workbook.getSheetAt(i);

        if (sheet == null) {
          continue;
        }

        Sheet excelSheet = createSheet(sheet);
        excelWorkbook.addSheet(excelSheet);

        SheetHeader header = sheetIndex2header.get(excelSheet.getIndex());
        if (header != null) {

          if (!header.isHasField()) {
            LOGGER.error("sheet header error, not has field row");
            throw new ExcelReadException("sheet header error, not has field row");
          }

          excelSheet.setHeader(header);
        }

        org.apache.poi.ss.usermodel.Row fieldRow = sheet.getRow(excelSheet.getHeader().getFieldRowIndex() - 1);
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

  private static Map<Integer, SheetHeader> buildHeaderMap(SheetHeader[] sheetHeaders) {
    Map<Integer, SheetHeader> sheetIndex2header = new HashMap<>();

    if (sheetHeaders != null) {
      for (SheetHeader header : sheetHeaders) {
        sheetIndex2header.put(header.getSheetIndex(), header);
      }
    }

    return sheetIndex2header;
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
