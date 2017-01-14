package spreadsheet.mapper.w2o.process.factory;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.*;

/**
 * default sheet meta factory
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class DefaultSheetMetaFactory implements SheetMetaFactory {

  private int dataStartRowIndex;

  private int fieldRowIndex;

  private int[] headerRowIndices = new int[0];

  /**
   * if you want specify the field row is header, should pass {@link #headerRowIndices} with {@link #fieldRowIndex}
   *
   * @param fieldRowIndex     which row is field
   * @param dataStartRowIndex data start at which row
   * @param headerRowIndices  which rows is headers, maybe no headers
   */
  public DefaultSheetMetaFactory(int fieldRowIndex, int dataStartRowIndex, int... headerRowIndices) {
    this.dataStartRowIndex = dataStartRowIndex;
    this.fieldRowIndex = fieldRowIndex;
    if (headerRowIndices != null) {
      this.headerRowIndices = headerRowIndices;
    }
  }

  @Override
  public SheetMeta create(Sheet sheet) {

    SheetMeta sheetMeta = new SheetMetaBean(sheet.getName(), dataStartRowIndex);

    Row fieldRow = sheet.getRow(fieldRowIndex);

    for (int i = 1; i <= fieldRow.sizeOfCells(); i++) {

      Cell fieldCell = fieldRow.getCell(i);
      if (StringUtils.isBlank(fieldCell.getValue())) {
        continue;
      }

      FieldMeta fieldMeta = new FieldMetaBean(fieldCell.getValue(), i);

      for (int headerRowIndex : headerRowIndices) {
        Cell headerCell = sheet.getRow(headerRowIndex).getCell(i);
        if (StringUtils.isBlank(headerCell.getValue())) {
          continue;
        }

        HeaderMeta headerMeta = new HeaderMetaBean(headerRowIndex, headerCell.getValue());
        fieldMeta.addHeaderMeta(headerMeta);
      }

      sheetMeta.addFieldMeta(fieldMeta);
    }

    return sheetMeta;

  }
}
