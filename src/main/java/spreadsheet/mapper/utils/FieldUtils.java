package spreadsheet.mapper.utils;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * object field utils
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class FieldUtils {

  private FieldUtils() {
    // default constructor
  }

  /**
   * <pre>
   * the business key present a domain object, it can identified a domain object.
   * it useful where update a domain object.
   * </pre>
   */
  public static final String BUSINESS_KEY_PREFIX = "businessKey.";

  /**
   * get field with out business key({@link #BUSINESS_KEY_PREFIX})
   *
   * @param field field
   * @return field without business key
   */
  public static String subtractBusinessKey(String field) {
    if (!StringUtils.contains(field, BUSINESS_KEY_PREFIX)) {
      throw new IllegalStateException("field is not business key");
    }
    return field.substring(BUSINESS_KEY_PREFIX.length());
  }

  /**
   * <pre>
   * get real field name from field meta name, without prefix if has prefix
   *
   * eg:
   * object.name -&gt; name
   * object.nested.name -&gt; nested.name
   * </pre>
   *
   * @param fieldMeta {@link FieldMeta}
   * @return real field name of object
   */
  public static String detectRealFieldName(FieldMeta fieldMeta) {

    if (StringUtils.isBlank(fieldMeta.getPrefix())) {
      return fieldMeta.getName();
    }

    String fieldName = StringUtils.substring(fieldMeta.getName(), fieldMeta.getPrefix().length());

    if (fieldName.contains(BUSINESS_KEY_PREFIX)) {
      fieldName = subtractBusinessKey(fieldName);
    }

    List<String> splitFields = new ArrayList<>(Arrays.asList(fieldName.split("\\.")));
    if (splitFields.size() == 1) {
      return splitFields.get(0);
    } else {
      splitFields.remove(0);
      return StringUtils.join(splitFields, Constants.DOT_SEPARATOR);
    }
  }

  /**
   * <pre>
   * get field if can't find return null.
   * supported private, private final, protected, protected final, public, public final.
   * </pre>
   *
   * @param clazz clazz
   * @param field field name of object
   * @return {@link Field}
   */
  public static Field getField(Class clazz, String field) {
    try {
      return clazz.getDeclaredField(field);
    } catch (NoSuchFieldException e) {
      if (clazz.getSuperclass() != null && !clazz.getSuperclass().equals(Object.class)) {
        return getField(clazz.getSuperclass(), field);
      }
    }
    return null;
  }

  /**
   * get field type if can't find return null.
   *
   * @param clazz  clazz
   * @param fields eg: [objectB, objectA, name] is objectB.objectA.name
   * @return {@link Field#getType()}
   */
  public static Class getFieldType(Class clazz, String[] fields) {
    if (fields.length == 0 || clazz == null) {
      return null;
    }
    if (fields.length > 1) {
      String[] newFields = new String[fields.length - 1];
      System.arraycopy(fields, 1, newFields, 0, newFields.length);
      return getFieldType(getFieldType(clazz, fields[0]), newFields);
    }
    return getFieldType(clazz, fields[0]);
  }

  /**
   * @param clazz     clazz
   * @param fieldName field name of object
   * @return {@link Field#getType()}
   * @see #getFieldType(Class, String[])
   */
  public static Class getFieldType(Class clazz, String fieldName) {

    Field field = getField(clazz, fieldName);

    if (field == null) {
      return null;
    }

    return field.getType();
  }
}
