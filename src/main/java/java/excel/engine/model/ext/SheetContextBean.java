package java.excel.engine.model.ext;

import java.excel.engine.exception.ExcelTemplateException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by hanwen on 2016/12/28.
 */
public class SheetContextBean implements SheetContext {

  private int sheetIndex;

  private String sheetName;

  private int dataStartRowIndex;

  private transient List<Object> data;

  private List<String> fields = new ArrayList<>();

  private Map<Integer, SheetHeader> rowIndex2sheetHeader = new HashMap<>();

  public SheetContextBean(int dataStartRowIndex, List<Object> data, List<String> fields) {
    this.sheetIndex = 1;
    this.dataStartRowIndex = dataStartRowIndex;
    this.data = data;
    this.fields = fields;
  }

  public SheetContextBean(int sheetIndex, int dataStartRowIndex, List<Object> data, List<String> fields) {
    this.sheetIndex = sheetIndex;
    this.data = data;
    this.dataStartRowIndex = dataStartRowIndex;
    this.fields = fields;
  }

  /**
   * the fields sequence present cell sequence in row (include data of field and header of field)
   *
   * @param sheetIndex        which sheet
   * @param sheetName         sheet name
   * @param dataStartRowIndex data start row index
   * @param data              data
   * @param fields            fields
   */
  public SheetContextBean(int sheetIndex, String sheetName, int dataStartRowIndex, List<Object> data, List<String> fields) {
    this.sheetIndex = sheetIndex;
    this.sheetName = sheetName;
    this.data = data;
    this.dataStartRowIndex = dataStartRowIndex;
    this.fields = fields;
  }

  @Override
  public void addHeader(SheetHeader sheetHeader) {
    rowIndex2sheetHeader.put(sheetHeader.getHeaderMeta().getRowIndex(), sheetHeader);
  }

  @Override
  public void addHeader(HeaderMeta meta, String... values) {
    if (CollectionUtils.isEmpty(fields)) {
      throw new ExcelTemplateException("fields can not be null");
    }

    if (values.length != fields.size()) {
      throw new ExcelTemplateException("supplied values size not equals fields size, supplied values can not corresponding to field");
    }

    SheetHeader sheetHeader = new SheetHeaderBean(meta);
    for (int i = 0; i < fields.size(); i++) {
      sheetHeader.addValue(fields.get(i), values[i]);
    }
    rowIndex2sheetHeader.put(sheetHeader.getHeaderMeta().getRowIndex(), sheetHeader);
  }

  @Override
  public void setSheetHeaders(List<SheetHeader> sheetHeaders) {
    for (SheetHeader sheetHeader : sheetHeaders) {
      rowIndex2sheetHeader.put(sheetHeader.getHeaderMeta().getRowIndex(), sheetHeader);
    }
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public String getSheetName() {
    return sheetName;
  }

  @Override
  public int getDataStartRowIndex() {
    return dataStartRowIndex;
  }

  @Override
  public List<Object> getData() {
    return data;
  }

  @Override
  public SheetTemplate ofTemplate() {

    if (MapUtils.isEmpty(rowIndex2sheetHeader)) {
      return new SheetTemplateBean(sheetIndex, dataStartRowIndex);
    }

    List<HeaderMeta> metas = new ArrayList<>();
    for (SheetHeader sheetHeader : rowIndex2sheetHeader.values()) {
      metas.add(sheetHeader.getHeaderMeta());
    }

    return new SheetTemplateBean(sheetIndex, dataStartRowIndex, metas.toArray(new HeaderMeta[0]));
  }

  @Override
  public List<String> getFields() {
    return fields;
  }

  @Override
  public List<SheetHeader> getSheetHeaders() {
    return new ArrayList<>(rowIndex2sheetHeader.values());
  }
}
